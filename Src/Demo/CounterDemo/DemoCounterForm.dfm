object FormDemoCounter: TFormDemoCounter
  Left = 0
  Top = 0
  Caption = 'DemoCounter'
  ClientHeight = 120
  ClientWidth = 143
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelCounter: TLabel
    Left = 64
    Top = 53
    Width = 12
    Height = 13
    Caption = '...'
  end
  object ButtonInc: TButton
    Left = 40
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Inc'
    TabOrder = 0
    OnClick = ButtonIncClick
  end
  object ButtonDec: TButton
    Left = 40
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Dec'
    TabOrder = 1
    OnClick = ButtonDecClick
  end
end
