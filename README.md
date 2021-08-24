The **DVD Chief** template engine used in generation of movie and person template and html export. The template engine has most features of **Smarty PHP** template engine.

Source: http://dvdchief.com/delphi

## Compilers ##

Tested with RAD XE2, RAD 10.3 and FPC 3.3.1.

This repo contains some modifications to the original sources:

## Changed ##

- `TVariableRecord` can typecast from `Variant`

- In `TNamespaceProvider`, pure callback mechanism of returning key options changed to hybrid pre-set/callback one. Initial options could be set in constructor and descendants are free to override getter methods. `GetName`, `IsIndexSupported`, `UseCache` are not abstract anymore, they return field values now. This allows to skip declaration of `TNamespaceProvider` descendant class in most cases.

- `TVariableArrayItem.Key` is string instead of Pointer

- Only Int64 numbers are used instead of Integer. `TVariableRecord.As/Is/Set/ToInteger` => `.As/Is/Set/ToInt`

- All string and record parameters are `const` for better performance. Many parameters marked as `out` instead of `var`.

- `TVariableRecord` class operators removed (wasn't used)

## Added ##

- `TStorageNamespaceProvider` - a `TNamespaceProvider` descendant that can store variables (both single and arrays) inside so there's no need in overriding `GetVariable` method.

- Helper routines `Arr` (pack array of values into single `TVariableRecord`), `Item` and `Map` (create key-value pairs into single `TVariableRecord`). Together they help creating complex data structures from constant parameters:

```delphi
  Namesp.SetVariable('Bands',
    Arr([
      Map([
        Item('Name', 'Manowar'),
        Item('Year', 1980),
        Item('Genre', ['Heavy metal', 'Epic metal']),
        Item('Lineup',
          [
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

- `TSmartyEngine.Execute` overloaded version - a somewhat simplified wrapper

- `SmartyExec` - even more simplified, a function that hides almost all engine interface

- Implemented option for engine to strip line breaks after block tags (`if`, `foreach` etc): `TSmartyEngine.StripLineBreaksAfterBlocks` property

- Unit-local `FormatSettings` and API for changing it: `SmartyDefaultFormatSettings`, `SmartyGetFormatSettings`, `SmartySetFormatSettings`. No more dependency on global `SysUtils.FormatSettings`. By default FormatSettings are invariant having following values:
```
  TimeSeparator := ':';
  ShortTimeFormat := 'hh:nn';
  LongTimeFormat := 'hh:nn:ss';
  DateSeparator := '.';
  ShortDateFormat := 'dd/MM/yyyy';
  LongDateFormat := 'dd/MM/yyyy HH:mm:ss';
  DecimalSeparator := '.';
```
- Unit test project

# Status #

I'm not going to develop this code actively since I'm not using it much and everything I needed is done already. But PRs are welcome as well as any tests!

# Contributions #

If you find this project useful, you can help by adding tests. Test fixtures are located in Test\TestCases.lst, format is described at the beginning of the file. Many modifiers and functions are waiting to be covered.

# License #

Initial license is dual GPL / MPL 1.0, as mentioned in TemplateEngine.pas, now GPL / MPL 2.0.
