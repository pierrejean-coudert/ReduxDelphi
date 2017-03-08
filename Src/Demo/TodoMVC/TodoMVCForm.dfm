object FormTodo: TFormTodo
  Left = 0
  Top = 0
  Caption = 'FormTodo'
  ClientHeight = 337
  ClientWidth = 437
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
      Left = 176
      Top = 10
      Width = 45
      Height = 19
      Caption = 'todos'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object CheckBoxAll: TCheckBox
      Left = 48
      Top = 40
      Width = 25
      Height = 17
      TabOrder = 0
      OnClick = CheckBoxAllClick
    end
    object EditTodo: TEdit
      Left = 64
      Top = 38
      Width = 329
      Height = 21
      TabOrder = 1
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
      Left = 48
      Top = 6
      Width = 345
      Height = 219
      OnClickCheck = CheckListBoxTodoClickCheck
      ItemHeight = 13
      Items.Strings = (
        'asasaa'
        'wsaasdsa'
        'assadasas')
      PopupMenu = PopupMenuDelete
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
    object LabelLeft: TLabel
      Left = 16
      Top = 16
      Width = 48
      Height = 13
      Cursor = crHandPoint
      Caption = 'x item left'
    end
    object LabelFilterAll: TLabel
      Left = 128
      Top = 16
      Width = 11
      Height = 13
      Cursor = crHandPoint
      Caption = 'All'
      OnClick = LabelFilterAllClick
    end
    object LabelFilterActive: TLabel
      Left = 160
      Top = 16
      Width = 30
      Height = 13
      Cursor = crHandPoint
      Caption = 'Active'
      OnClick = LabelFilterActiveClick
    end
    object LabelFilterCompleted: TLabel
      Left = 208
      Top = 16
      Width = 51
      Height = 13
      Cursor = crHandPoint
      Caption = 'Completed'
      OnClick = LabelFilterCompletedClick
    end
    object LabelClearCompleted: TLabel
      Left = 341
      Top = 14
      Width = 77
      Height = 13
      Cursor = crHandPoint
      Caption = 'Clear completed'
      OnClick = LabelClearCompletedClick
    end
  end
  object PopupMenuDelete: TPopupMenu
    Left = 408
    Top = 177
    object MenuItemDelete: TMenuItem
      Caption = 'Delete'
      OnClick = MenuItemDeleteClick
    end
  end
end
