object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'DVD Chief Template Engine Demo'
  ClientHeight = 432
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object spl: TSplitter
    Left = 400
    Top = 0
    Height = 432
    ExplicitLeft = 512
    ExplicitTop = 192
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 400
    Height = 432
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 0
    object memParse: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 38
      Width = 390
      Height = 389
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
        'Item is - {$item}, key is - {$key}, current iteration - '
        '{$smarty.foreach.current.iteration}'
        '{/foreach}')
      TabOrder = 0
    end
    object Parse: TButton
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 390
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Parse'
      TabOrder = 1
      OnClick = ParseClick
    end
  end
  object memResult: TMemo
    Left = 403
    Top = 0
    Width = 282
    Height = 432
    Align = alClient
    Lines.Strings = (
      'memResult')
    TabOrder = 1
  end
end
