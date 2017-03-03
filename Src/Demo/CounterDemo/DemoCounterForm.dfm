object FormDemoCounter: TFormDemoCounter
  Left = 0
  Top = 0
  Caption = 'DemoCounter'
  ClientHeight = 120
  ClientWidth = 427
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
  object Label1: TLabel
    Left = 136
    Top = 32
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object ButtonInc: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Inc'
    TabOrder = 0
    OnClick = ButtonIncClick
  end
  object ButtonDec: TButton
    Left = 16
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Dec'
    TabOrder = 1
    OnClick = ButtonDecClick
  end
  object Memo1: TMemo
    Left = 200
    Top = 8
    Width = 185
    Height = 89
    TabOrder = 2
  end
end
