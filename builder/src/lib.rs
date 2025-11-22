use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, Field, Ident, Type};

enum BuilderDeriveField {
	Required {
		ident: Ident,
		ty: Type,
	},
	Optional {
		ident: Ident,
		ty: Type,
	},
	Each {
		ident: Ident,
		each_ident: Ident,
		ty: Type,
	},
}

enum BuilderDeriveFieldTryFromError {
	Recoverable,
	Unrecoverable(syn::Error),
}

impl From<syn::Error> for BuilderDeriveFieldTryFromError {
	fn from(value: syn::Error) -> Self {
		Self::Unrecoverable(value)
	}
}

impl BuilderDeriveField {
	fn ident(&self) -> &Ident {
		match self {
			Self::Required { ident, .. } => ident,
			Self::Optional { ident, .. } => ident,
			Self::Each { ident, .. } => ident,
		}
	}

	fn ty(&self) -> &Type {
		match self {
			Self::Required { ty, .. } => ty,
			Self::Optional { ty, .. } => ty,
			Self::Each { ty, .. } => ty,
		}
	}

	fn try_from_option(field: &Field) -> Result<Self, BuilderDeriveFieldTryFromError> {
		let Field { ident, ty, .. } = field;
		let ident = ident
			.as_ref()
			.ok_or(BuilderDeriveFieldTryFromError::Recoverable)?;

		let ty_inner = Self::try_from_inner(ty, |segments| match &*segments {
			[a] => {
				matches!(a.as_str(), "Option")
			}
			[a, b] => {
				matches!((a.as_str(), b.as_str()), ("option", "Option"))
			}
			[a, b, c] => {
				matches!(
					(a.as_str(), b.as_str(), c.as_str()),
					("std" | "core", "option", "Option")
				)
			}
			_ => false,
		})
		.ok_or(BuilderDeriveFieldTryFromError::Recoverable)?;

		Ok(Self::Optional {
			ident: ident.clone(),
			ty: ty_inner.clone(),
		})
	}

	fn try_from_vec(field: &Field) -> Result<Self, BuilderDeriveFieldTryFromError> {
		let Field {
			ident, ty, attrs, ..
		} = field;
		let ident = ident
			.as_ref()
			.ok_or(BuilderDeriveFieldTryFromError::Recoverable)?;

		let ty_inner = Self::try_from_inner(ty, |segments| match &*segments {
			[a] => {
				matches!(a.as_str(), "Vec")
			}
			[a, b] => {
				matches!((a.as_str(), b.as_str()), ("vec", "Vec"))
			}
			[a, b, c] => {
				matches!((a.as_str(), b.as_str(), c.as_str()), ("std", "vec", "Vec"))
			}
			_ => false,
		})
		.ok_or(BuilderDeriveFieldTryFromError::Recoverable)?;

		let each = attrs
			.iter()
			.find_map(|attr| {
				if !matches!(attr.style, syn::AttrStyle::Outer) {
					return None;
				}

				let list = match attr {
					syn::Attribute {
						meta: syn::Meta::List(list),
						..
					} => list,
					_ => return None,
				};

				let syn::ExprAssign { left, right, .. } = list.parse_args().ok()?;

				let syn::Expr::Path(left) = *left else {
					return None;
				};

				if left.path.segments.len() != 1 || left.path.segments[0].ident.to_string() != "each" {
					return Some(Err(syn::Error::new_spanned(
						list,
						"expected `builder(each = \"...\")`",
					)));
				}

				let syn::Expr::Lit(syn::ExprLit {
					lit: syn::Lit::Str(right),
					..
				}) = *right
				else {
					return None;
				};

				Some(Ok(right))
			})
			.ok_or(BuilderDeriveFieldTryFromError::Recoverable)??;

		Ok(Self::Each {
			ident: ident.clone(),
			each_ident: Ident::new(&each.value(), each.span()),
			ty: ty_inner.clone(),
		})
	}

	fn try_from_inner<F>(ty: &Type, check_path: F) -> Option<&Type>
	where
		F: (FnOnce(Vec<String>) -> bool),
	{
		let Type::Path(syn::TypePath {
			path: syn::Path { segments, .. },
			..
		}) = ty
		else {
			return None;
		};

		if !check_path(segments.iter().map(|seg| seg.ident.to_string()).collect()) {
			return None;
		}

		let syn::PathArguments::AngleBracketed(arguments) = &segments.last()?.arguments else {
			return None;
		};

		let arguments = &arguments.args;

		if arguments.len() != 1 {
			return None;
		}

		let syn::GenericArgument::Type(ty_inner) = arguments.get(0)? else {
			return None;
		};

		Some(ty_inner)
	}
}

impl TryFrom<Field> for BuilderDeriveField {
	type Error = syn::Error;

	fn try_from(field: Field) -> Result<Self, Self::Error> {
		match Self::try_from_option(&field) {
			Ok(bdf) => return Ok(bdf),
			Err(BuilderDeriveFieldTryFromError::Unrecoverable(e)) => {
				return Err(e);
			}
			_ => {}
		}

		match Self::try_from_vec(&field) {
			Ok(bdf) => return Ok(bdf),
			Err(BuilderDeriveFieldTryFromError::Unrecoverable(e)) => {
				return Err(e);
			}
			_ => {}
		}

		let Field { ident, ty, .. } = &field;
		let ident = ident
			.as_ref()
			.ok_or(syn::Error::new_spanned(field.clone(), "No ident"))?;

		Ok(Self::Required {
			ident: ident.clone(),
			ty: ty.clone(),
		})
	}
}

use BuilderDeriveField as BDF;

struct BuilderDerive {
	pub ident: Ident,
	pub fields: Vec<BDF>,
}

impl TryFrom<DeriveInput> for BuilderDerive {
	type Error = syn::Error;

	fn try_from(input: DeriveInput) -> Result<Self, Self::Error> {
		let syn::Data::Struct(syn::DataStruct {
			fields: syn::Fields::Named(fields),
			..
		}) = input.data
		else {
			panic!("Could not parse");
		};

		fields
			.named
			.into_iter()
			.map(|field| field.try_into())
			.collect::<Result<Vec<_>, _>>()
			.map(|fields| Self {
				ident: input.ident,
				fields,
			})
	}
}

impl quote::ToTokens for BuilderDerive {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		let ident = &self.ident;
		let builder_ident = Ident::new(&format!("{}Builder", ident), ident.span());

		let builder_fields = self.fields.iter().map(|field| {
			let ident = field.ident();
			let ty = field.ty();

			match field {
				BDF::Each { ty, .. } => quote! { #ident: ::std::vec::Vec<#ty>, },
				_ => quote! { #ident: ::core::option::Option<#ty>, },
			}
		});

		let impl_builder_fields = self.fields.iter().map(|field| {
			let ident = field.ident();

			match field {
				BDF::Each { .. } => quote! { #ident: ::std::vec::Vec::new(), },
				_ => quote! { #ident: ::core::option::Option::None, },
			}
		});

		let methods = self.fields.iter().map(|field| {
			let ident = field.ident();
			let ty = field.ty();

			match field {
				BDF::Each { each_ident, .. } => {
					let each_method = quote! {
						fn #each_ident(&mut self, #each_ident: #ty) -> &mut Self {
							self.#ident.push(#each_ident);
							self
						}
					};

					if each_ident.to_string() == ident.to_string() {
						each_method
					} else {
						quote! {
						fn #ident(&mut self, #ident: ::std::vec::Vec<#ty>) -> &mut Self {
							self.#ident = #ident;
							self
						}

							#each_method
						}
					}
				}
				_ => quote! {
					fn #ident(&mut self, #ident: #ty) -> &mut Self {
						self.#ident = ::core::option::Option::Some(#ident);
						self
					}
				},
			}
		});

		let built_fields = self.fields.iter().map(|field| {
			let ident = field.ident();

			let error_text = format!("`{}` is not set", ident.to_string());

			match field {
				BDF::Required { .. } => quote! { #ident: self.#ident.take().ok_or(#error_text)?, },
				BDF::Optional { .. } => quote! { #ident: self.#ident.take(), },
				BDF::Each { .. } => quote! { #ident: self.#ident.drain(..).collect(), },
			}
		});

		tokens.extend(quote! {
			pub struct #builder_ident {
				#(#builder_fields)*
			}

			impl #builder_ident {
				#(#methods)*

				 pub fn build(&mut self) -> ::core::result::Result<#ident, ::std::boxed::Box<dyn ::core::error::Error>> {
					::core::result::Result::Ok(
						#ident{ #(#built_fields)* }
					)
				}
			}

			impl #ident {
				fn builder () -> #builder_ident {
					#builder_ident{
						#(#impl_builder_fields)*
					}
				}
			}
		});
	}
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
	let parsed = syn::parse_macro_input!(input as DeriveInput);

	match parsed.try_into() {
		Ok(bd @ BuilderDerive { .. }) => quote! { #bd }.into(),
		Err(e) => {
			let e = e.to_compile_error();
			quote! { #e }.into()
		}
	}
}
