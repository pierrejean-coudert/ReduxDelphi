object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 119
  ClientWidth = 501
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
    Left = 24
    Top = 35
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object LabelSaved: TLabel
    Left = 24
    Top = 93
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Edit1: TEdit
    Left = 24
    Top = 8
    Width = 193
    Height = 21
    TabOrder = 0
    OnChange = Edit1Change
  end
  object ButtonSave: TButton
    Left = 24
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 1
    OnClick = ButtonSaveClick
  end
end
