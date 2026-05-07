use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{
    parse_macro_input, Attribute, Data, DataStruct, DeriveInput, Expr, ExprLit, Fields,
    FieldsNamed, GenericArgument, Lit, MetaNameValue, PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let data = input.data;

    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());

    // Extracts only Named fields in the struct
    // TODO: add compiler errors for unit and tuple structs
    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = data
    {
        named
    } else {
        let error = syn::Error::new_spanned(name, "expected a `struct`");
        return error.to_compile_error().into();
    };

    // the body of builder() method
    let builder_body = fields.iter().map(|f| {
        let ident = &f.ident.as_ref().unwrap();
        if let Some(inner_type) = parse_inner_type(&f.ty, "Option") {
            quote! {
                #ident: <std::option::Option<#inner_type> as std::default::Default>::default()
            }
        } else {
            let ty = &f.ty;
            quote! {
                #ident: <std::option::Option<#ty> as std::default::Default>::default()
            }
        }
    });

    // the fields for struct, in this case, CommandBuilder
    let struct_builder_fields = fields.iter().map(|f| {
        let ident = &f.ident.as_ref().unwrap();
        if let Some(inner_type) = parse_inner_type(&f.ty, "Option") {
            quote! {
                #ident: std::option::Option<#inner_type>
            }
        } else {
            let ty = &f.ty;
            quote! {
                #ident: std::option::Option<#ty>
            }
        }
    });

    // the body of build() method
    let build_body = fields.iter().map(|f| {
        let ident = &f.ident.as_ref().unwrap();
        let ty = &f.ty;
        if parse_inner_type(ty, "Option").is_some() {
            quote! {
                #ident: self.#ident.clone()
            }
        } else {
            quote! {
                #ident: self.#ident.clone().unwrap_or_default()
            }
        }
    });

    let mut repeated_methods = Vec::new();
    let mut struct_builder_methods = Vec::new();

    for field in fields {
        let ident = &field.ident.as_ref().unwrap();
        let field_type = &field.ty;

        // no attributes
        if field.attrs.is_empty() {
            // still generate the methods for the builder struct
            if let Some(inner_type) = parse_inner_type(&field_type, "Option") {
                let method = quote! {
                    pub fn #ident(&mut self, #ident: #inner_type) -> &mut Self {
                        self.#ident = Some(#ident);
                        self
                    }
                };
                struct_builder_methods.push(method);
            } else {
                let method = quote! {
                    pub fn #ident(&mut self, #ident: #field_type) -> &mut Self {
                        self.#ident = Some(#ident);
                        self
                    }
                };
                struct_builder_methods.push(method);
            }

            continue;
        }

        // a single or more attributes
        // assuming the attribute wanted is the first one.
        if field.attrs.len() >= 1 {
            let attr = &field.attrs.first().unwrap();
            if attr.path().is_ident("builder") {
                let value = match parse_attributes(&attr) {
                    Ok(value) => value,
                    Err(err) => return err.to_compile_error().into(),
                };

                let ident_string = ident.to_string();
                let generate_all_in_one = value != ident_string;

                let new_ident = Ident::new(&value, Span::call_site());

                if let Some(inner_type) = parse_inner_type(&field_type, "Option") {
                    if generate_all_in_one {
                        let method = quote! {
                            pub fn #ident(&mut self, #ident: #inner_type) -> &mut Self {
                                self.#ident = Some(#ident);
                                self
                            }
                        };
                        struct_builder_methods.push(method);
                    }

                    if let Some(inner_type) = parse_inner_type(&inner_type, "Vec") {
                        let method = quote! {
                            pub fn #new_ident(&mut self, #ident: #inner_type) -> &mut Self {
                                match self.#ident {
                                    Some(ref mut inner) => inner.push(#ident),
                                    None => self.#ident = Some(vec![#ident]),
                                }
                                self
                            }
                        };
                        repeated_methods.push(method);
                    }
                } else {
                    if generate_all_in_one {
                        let method = quote! {
                            pub fn #ident(&mut self, #ident: #field_type) -> &mut Self {
                                self.#ident = Some(#ident);
                                self
                            }
                        };
                        struct_builder_methods.push(method);
                    }
                    if let Some(inner_type) = parse_inner_type(&field_type, "Vec") {
                        let method = quote! {
                            pub fn #new_ident(&mut self, #ident: #inner_type) -> &mut Self {
                                match self.#ident {
                                    Some(ref mut inner) => inner.push(#ident),
                                    None => self.#ident = Some(vec![#ident]),
                                }
                                self
                            }
                        };
                        repeated_methods.push(method);
                    }
                }
            }
        }
    }

    // The final generated TokenStream for the compiler.
    let generated = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_body),*
                }
            }
        }

        pub struct #builder_name {
            #(#struct_builder_fields),*
        }

        impl #builder_name {
            #(#struct_builder_methods)*

            #(#repeated_methods)*

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                let result = #name {
                    #(#build_body),*
                };

                Ok(result)
            }
        }
    };

    generated.into()
}

// parses the Type::Path and extracts type owned by Option and Vec
fn parse_inner_type<'t>(ty: &'t Type, ident: &str) -> std::option::Option<&'t Type> {
    if let Type::Path(TypePath { qself: None, path }) = ty {
        // the last() returns the last part of the path.
        // e.g. std::option::Option, it gives us the Option
        if let Some(last_segment) = path.segments.last() {
            let segment_ident = &last_segment.ident;
            if segment_ident == ident {
                if let PathArguments::AngleBracketed(args) = &last_segment.arguments {
                    if let Some(GenericArgument::Type(inner_type)) = args.args.first() {
                        return Some(&inner_type);
                    }
                }
            }
        }
    }

    None
}

fn parse_attributes(attr: &Attribute) -> syn::Result<String> {
    // Check if the attribute expected is of Meta::List
    // #[builder(...)]
    let meta = attr.meta.require_list()?;

    // And then, the attribute inside the Delimiters ()
    // is MetaNameValue
    // e.g. (each = "arg")
    let nested: MetaNameValue = meta.parse_args()?;

    if nested.path.is_ident("each") {
        let Expr::Lit(ExprLit {
            lit: Lit::Str(lit_str),
            ..
        }) = &nested.value
        else {
            return Err(syn::Error::new_spanned(
                nested.value,
                "expected a string literal",
            ));
        };
        Ok(lit_str.value())
    } else {
        Err(syn::Error::new_spanned(
            meta,
            "expected `builder(each = \"...\")`",
        ))
    }
}
