object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 337
  ClientWidth = 437
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
  object PanelHeaner: TPanel
    Left = 0
    Top = 0
    Width = 437
    Height = 65
    Align = alTop
    TabOrder = 0
    object LabelTitle: TLabel
      Left = 232
      Top = 8
      Width = 55
      Height = 19
      Caption = 'TODOs'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object CheckBoxAll: TCheckBox
      Left = 64
      Top = 40
      Width = 25
      Height = 17
      TabOrder = 0
    end
    object EditTodo: TEdit
      Left = 80
      Top = 38
      Width = 329
      Height = 21
      TabOrder = 1
      Text = 'EditTodo'
      OnKeyPress = EditTodoKeyPress
    end
  end
  object PanelTodos: TPanel
    Left = 0
    Top = 65
    Width = 437
    Height = 231
    Align = alClient
    TabOrder = 1
    object CheckListBoxTodo: TCheckListBox
      Left = 64
      Top = 6
      Width = 345
      Height = 219
      OnClickCheck = CheckListBoxTodoClickCheck
      ItemHeight = 13
      Items.Strings = (
        'asasaa'
        'wsaasdsa'
        'assadasas')
      TabOrder = 0
    end
  end
  object PanelFooter: TPanel
    Left = 0
    Top = 296
    Width = 437
    Height = 41
    Align = alBottom
    TabOrder = 2
  end
end
