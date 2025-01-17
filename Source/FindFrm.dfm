object FindForm: TFindForm
  Left = 662
  Top = 433
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Find Text'
  ClientHeight = 339
  ClientWidth = 353
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  PopupMenu = FindPopup
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    353
    339)
  PixelsPerInch = 96
  TextHeight = 13
  object btnExecute: TButton
    Left = 8
    Top = 309
    Width = 100
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Find'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnExecuteClick
    ExplicitTop = 300
  end
  object btnCancel: TButton
    Left = 222
    Top = 309
    Width = 100
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
    ExplicitTop = 300
  end
  object FindTabs: TTabControl
    Left = 0
    Top = 0
    Width = 353
    Height = 305
    Align = alTop
    TabOrder = 2
    Tabs.Strings = (
      'Find'
      'Find in files'
      'Replace'
      'Replace in files')
    TabIndex = 0
    OnChange = FindTabsChange
    object lblFind: TLabel
      Left = 8
      Top = 29
      Width = 56
      Height = 13
      Caption = '&Text to find:'
      FocusControl = cboFindText
    end
    object lblReplace: TLabel
      Left = 8
      Top = 71
      Width = 65
      Height = 13
      Caption = 'Replace with:'
      FocusControl = cboFindText
    end
    object cboFindText: TComboBox
      Left = 8
      Top = 46
      Width = 314
      Height = 21
      TabOrder = 0
      OnKeyUp = cboFindTextKeyUp
    end
    object grpOptions: TGroupBox
      Left = 8
      Top = 118
      Width = 152
      Height = 87
      Caption = '  Options:  '
      TabOrder = 2
      object cbMatchCase: TCheckBox
        Left = 8
        Top = 16
        Width = 120
        Height = 17
        Caption = 'C&ase sensitive'
        TabOrder = 0
      end
      object cbWholeWord: TCheckBox
        Left = 8
        Top = 40
        Width = 121
        Height = 17
        Caption = '&Whole words only'
        TabOrder = 1
      end
      object cbPrompt: TCheckBox
        Left = 8
        Top = 64
        Width = 120
        Height = 17
        Caption = '&Prompt on Replace'
        TabOrder = 2
      end
    end
    object grpDirection: TGroupBox
      Left = 16
      Top = 211
      Width = 144
      Height = 71
      Caption = '  Direction:  '
      TabOrder = 3
      object rbBackward: TRadioButton
        Left = 8
        Top = 42
        Width = 121
        Height = 17
        Caption = '&Backward'
        TabOrder = 0
      end
      object rbForward: TRadioButton
        Left = 8
        Top = 18
        Width = 121
        Height = 17
        Caption = '&Forward'
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object grpWhere: TGroupBox
      Left = 169
      Top = 118
      Width = 152
      Height = 87
      Caption = '  Where:  '
      TabOrder = 4
      object rbProjectFiles: TRadioButton
        Left = 8
        Top = 20
        Width = 121
        Height = 17
        Caption = 'Files in Project'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbOpenFiles: TRadioButton
        Left = 8
        Top = 41
        Width = 121
        Height = 17
        Caption = 'Open Files'
        TabOrder = 1
      end
      object rbCurFile: TRadioButton
        Left = 8
        Top = 64
        Width = 121
        Height = 17
        Caption = 'Current file'
        TabOrder = 2
      end
    end
    object grpScope: TGroupBox
      Left = 169
      Top = 211
      Width = 152
      Height = 70
      Caption = '  Scope:  '
      TabOrder = 5
      object rbGlobal: TRadioButton
        Left = 8
        Top = 18
        Width = 121
        Height = 17
        Caption = '&Global'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbSelectedOnly: TRadioButton
        Left = 8
        Top = 42
        Width = 121
        Height = 17
        Caption = '&Selected only'
        TabOrder = 1
      end
    end
    object grpOrigin: TGroupBox
      Left = 75
      Top = 157
      Width = 152
      Height = 86
      Caption = '  Origin:  '
      TabOrder = 6
      object rbFromCursor: TRadioButton
        Left = 8
        Top = 26
        Width = 121
        Height = 17
        Caption = 'From &cursor'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbEntireScope: TRadioButton
        Left = 8
        Top = 58
        Width = 121
        Height = 17
        Caption = 'Entire &scope'
        TabOrder = 1
      end
    end
    object cboReplaceText: TComboBox
      Left = 8
      Top = 88
      Width = 314
      Height = 21
      TabOrder = 1
      OnKeyUp = cboReplaceTextKeyUp
    end
  end
  object FindPopup: TPopupMenu
    Left = 288
    Top = 240
    object FindCut: TMenuItem
      Caption = 'Cut'
      ShortCut = 16472
      OnClick = FindCutClick
    end
    object FindCopy: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = FindCopyClick
    end
    object FindPaste: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = FindPasteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object FindSelAll: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = FindSelAllClick
    end
  end
end
