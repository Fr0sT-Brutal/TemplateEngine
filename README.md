The **DVD Chief** template engine used in generation of movie and person template and html export. The template engine has most features of **Smarty PHP** template engine.

Source: http://dvdchief.com/delphi

This repo contains some modifications to the original sources:

- `TVariableRecord` can typecast from `Variant`
	
- `TStorageNamespaceProvider` - a `TNamespaceProvider` descendant that can store variables (both single and arrays) inside so there's no need in overriding `GetVariable` method.

- In `TNamespaceProvider`, pure callback mechanism of returning key options changed to hybrid pre-set/callback one. Initial options could be set in constructor and descendants are free to override getter methods. `GetName`, `IsIndexSupported`, `UseCache` are not abstract anymore, they return field values now. This allows to skip declaration of `TNamespaceProvider` descendant class in most cases.

- Helper routines `Arr` (pack array of values into single `TVariableRecord`), `Item` and `Map` (create key-value pairs into single `TVariableRecord`). Together they help creating complex data structures from constant parameters:

```delphi
  Namesp.SetVariable('Bands',
    Arr([
      Map([
        Item('Name', 'Manowar'),
        Item('Year', 1980),
        Item('Genre', 'Heavy metal'),
        Item('Lineup',
          Arr([
            Map([
              Item('Name', 'Eric Adams'),
              Item('Instrument', 'Vocals')
            ]),
            Map([
              Item('Name', 'Joey DeMaio'),
              Item('Instrument', 'Bass')
            ]),
            ...
          ])
        )
      ]),
      ...
    ])
  );
```