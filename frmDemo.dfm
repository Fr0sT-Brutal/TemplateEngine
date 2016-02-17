object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'DVD Chief Template Engine Demo'
  ClientHeight = 565
  ClientWidth = 896
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 17
  object spl: TSplitter
    Left = 523
    Top = 0
    Width = 4
    Height = 565
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 523
    Height = 565
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 0
    object memParse: TMemo
      AlignWithMargins = True
      Left = 6
      Top = 63
      Width = 511
      Height = 496
      Margins.Left = 5
      Margins.Top = 20
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Lines.Strings = (
        '{literal}Here you can use any symbols ;) {}{} {/literal}'
        'Simple variable output: {$mynamespace.string}'
        'Output with modifiers: {$mynamespace.string | upper}'
        'Function output: {upper_case($mynamespace.string)}'
        'Conditional output: {if $mynamespace.string = "string"}'
        'String is equal to string {else}Oh, no{/if}'
        'Array functions:'
        '{foreach from=$mynamespace.array item=item key=key}'
        
          'Item is - {$item}, key is - {$key}, current iteration - {$smarty' +
          '.foreach.current.iteration}'
        '{/foreach}'
        'Nested array functions:'
        '{foreach from=$mynamespace.nested_array item=item}'
        
          'Item is - {$item.name}, current iteration - {$smarty.foreach.cur' +
          'rent.iteration}'
        '{foreach from=$item.subitems item=subitem}'
        
          #9'SubItem is - {$item.name}.{$subitem}, current iteration - {$sma' +
          'rty.foreach.current.iteration}'
        '{/foreach}'
        '{/foreach}')
      TabOrder = 0
      WordWrap = False
    end
    object Parse: TButton
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 511
      Height = 32
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Caption = 'Parse'
      TabOrder = 1
      OnClick = ParseClick
    end
    object chbNewMethod: TCheckBox
      Left = 8
      Top = 40
      Width = 241
      Height = 17
      Caption = 'Use new simple variable assignment'
      TabOrder = 2
    end
  end
  object memResult: TMemo
    Left = 535
    Top = 0
    Width = 455
    Height = 565
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Lines.Strings = (
      'memResult')
    TabOrder = 1
  end
end
