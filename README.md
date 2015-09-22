The **DVD Chief** template engine used in generation of movie and person template and html export. The template engine has most features of **Smarty PHP** template engine.

Source: http://dvdchief.com/delphi

This repo contains some modifications to the original sources:

- `TVariableRecord` can typecast from `Variant`
	
- `TStorageNamespaceProvider` - a `TNamespaceProvider` descendant that can store variables (both single and arrays) inside so there's no need in overriding `GetVariable` method.
