object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 337
  ClientWidth = 635
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
    Left = 56
    Top = 72
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object LabelSaved: TLabel
    Left = 304
    Top = 35
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Edit1: TEdit
    Left = 56
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object ButtonSave: TButton
    Left = 208
    Top = 30
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 1
    OnClick = ButtonSaveClick
  end
end
