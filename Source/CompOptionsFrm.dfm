object CompOptForm: TCompOptForm
  Left = 1053
  Top = 848
  BorderStyle = bsSizeToolWin
  Caption = 'Compiler options'
  ClientHeight = 513
  ClientWidth = 482
  Color = clWindow
  Constraints.MaxHeight = 560
  Constraints.MinHeight = 560
  Constraints.MinWidth = 500
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = InterfaceSave
  DesignSize = (
    482
    513)
  PixelsPerInch = 96
  TextHeight = 15
  object btnOk: TBitBtn
    Left = 210
    Top = 481
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Images = dmMain.SVGImageListMessageStyle
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
    ExplicitTop = 480
  end
  object btnCancel: TBitBtn
    Left = 300
    Top = 481
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = '&Cancel'
    Images = dmMain.SVGImageListMenuStyle
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
    ExplicitTop = 480
  end
  object btnHelp: TBitBtn
    Left = 390
    Top = 481
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Help'
    Enabled = False
    TabOrder = 3
    OnClick = btnHelpClick
    ExplicitTop = 480
  end
  object MainPages: TPageControl
    Tag = 2
    Left = 0
    Top = 70
    Width = 478
    Height = 405
    ActivePage = tabCompiler
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    ExplicitWidth = 484
    object tabCompiler: TTabSheet
      Caption = 'General'
      ExplicitWidth = 476
      DesignSize = (
        470
        375)
      object cbCompAdd: TCheckBox
        Left = 10
        Top = 6
        Width = 444
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Add the following commands when calling the compiler:'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        OnClick = cbCompAddClick
        ExplicitWidth = 450
      end
      object Commands: TMemo
        Left = 19
        Top = 32
        Width = 443
        Height = 150
        Anchors = [akLeft, akRight]
        Ctl3D = False
        ParentCtl3D = False
        ScrollBars = ssVertical
        TabOrder = 1
        WantReturns = False
        OnChange = InterfaceChange
      end
      object cbLinkerAdd: TCheckBox
        Left = 10
        Top = 186
        Width = 444
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Add these commands to the linker command line'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
        OnClick = cbLinkerAddClick
        ExplicitWidth = 450
      end
      object Linker: TMemo
        Left = 19
        Top = 212
        Width = 443
        Height = 150
        Anchors = [akLeft, akRight]
        Ctl3D = False
        ParentCtl3D = False
        ScrollBars = ssVertical
        TabOrder = 3
        WantReturns = False
        OnChange = InterfaceChange
      end
    end
    object tabCodeGen: TTabSheet
      Caption = 'Settings'
      ExplicitWidth = 476
      object OptionsTip: TLabel
        Left = 0
        Top = 336
        Width = 476
        Height = 20
        Alignment = taCenter
        AutoSize = False
        Caption = 'For more information about GCC'#39's options, please visit'
      end
      object OptionsLink: TLabel
        Left = 0
        Top = 355
        Width = 476
        Height = 20
        Cursor = crHandPoint
        Alignment = taCenter
        AutoSize = False
        Caption = 'http://gcc.gnu.org/onlinedocs/gcc/Option-Summary.html'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentColor = False
        ParentFont = False
        OnClick = OptionsLinkClick
      end
      inline CompOptionsFrame1: TCompOptionsFrame
        Left = 0
        Top = 0
        Width = 476
        Height = 333
        HorzScrollBar.Visible = False
        VertScrollBar.Visible = False
        TabOrder = 0
        ExplicitWidth = 476
        ExplicitHeight = 333
        inherited tabs: TTabControl
          Width = 476
          Height = 333
          ExplicitWidth = 476
          ExplicitHeight = 333
          inherited vle: TCompOptionsList
            Width = 468
            Height = 323
            ExplicitWidth = 410
            ExplicitHeight = 323
          end
        end
      end
    end
    object tabDirectories: TTabSheet
      Caption = 'Directories'
      ExplicitWidth = 476
      object DirTabs: TTabControl
        Left = 0
        Top = 0
        Width = 470
        Height = 375
        Align = alClient
        TabOrder = 0
        Tabs.Strings = (
          'Binaries'
          'Libraries'
          'C Includes'
          'C++ Includes')
        TabIndex = 0
        OnChange = DirTabsChange
        ExplicitWidth = 476
        DesignSize = (
          470
          375)
        object btnUp: TSpeedButton
          Left = 439
          Top = 140
          Width = 23
          Height = 22
          Anchors = [akRight, akBottom]
          ImageIndex = 56
          ImageName = 'iconsnew-52'
          Images = dmMain.SVGImageListMenuStyle
          Enabled = False
          Flat = True
          OnClick = UpDownClick
          ExplicitLeft = 445
        end
        object btnDown: TSpeedButton
          Left = 439
          Top = 170
          Width = 23
          Height = 22
          Anchors = [akRight, akBottom]
          ImageIndex = 57
          ImageName = 'iconsnew-53'
          Images = dmMain.SVGImageListMenuStyle
          Enabled = False
          Flat = True
          OnClick = UpDownClick
          ExplicitLeft = 445
        end
        object btnBrowse: TSpeedButton
          Left = 442
          Top = 310
          Width = 23
          Height = 22
          Anchors = [akTop, akRight]
          ImageIndex = 59
          ImageName = 'iconsnew-65'
          Images = dmMain.SVGImageListMenuStyle
          Flat = True
          OnClick = btnBrowseClick
        end
        object lstDirs: TListBox
          Left = 4
          Top = 26
          Width = 425
          Height = 280
          Anchors = [akLeft, akTop, akRight]
          Ctl3D = False
          ItemHeight = 15
          ParentCtl3D = False
          ScrollWidth = 600
          TabOrder = 0
          OnClick = lstDirsClick
          OnDblClick = lstDirsDblClick
          ExplicitWidth = 435
        end
        object edEntry: TEdit
          Left = 4
          Top = 310
          Width = 429
          Height = 21
          Anchors = [akLeft, akRight, akBottom]
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 1
          OnChange = edEntryChange
          OnKeyUp = edEntryKeyUp
          ExplicitWidth = 435
        end
        object btnDelInval: TButton
          Tag = 4
          Left = 334
          Top = 343
          Width = 100
          Height = 23
          Anchors = [akLeft, akBottom]
          Caption = 'Delete &Invalid'
          TabOrder = 5
          OnClick = ButtonClick
        end
        object btnDelete: TButton
          Tag = 3
          Left = 224
          Top = 343
          Width = 100
          Height = 23
          Anchors = [akLeft, akBottom]
          Caption = '&Delete'
          Enabled = False
          TabOrder = 4
          OnClick = ButtonClick
        end
        object btnAdd: TButton
          Tag = 2
          Left = 114
          Top = 343
          Width = 100
          Height = 23
          Anchors = [akLeft, akBottom]
          Caption = '&Add'
          Enabled = False
          TabOrder = 3
          OnClick = ButtonClick
        end
        object btnReplace: TButton
          Tag = 1
          Left = 4
          Top = 343
          Width = 100
          Height = 23
          Anchors = [akLeft, akBottom]
          Caption = '&Replace'
          Enabled = False
          TabOrder = 2
          OnClick = ButtonClick
        end
      end
    end
    object tabPrograms: TTabSheet
      Caption = 'Programs'
      ExplicitWidth = 476
      DesignSize = (
        470
        375)
      object lblProgramsText: TLabel
        Left = 24
        Top = 16
        Width = 398
        Height = 30
        Caption = 
          'You may want to change the programs filenames that are used in D' +
          'ev-C++ (for example when using a cross compiler):'
        WordWrap = True
      end
      object lblgcc: TLabel
        Left = 24
        Top = 72
        Width = 25
        Height = 15
        Caption = 'gcc: '
      end
      object lblgpp: TLabel
        Left = 24
        Top = 116
        Width = 29
        Height = 15
        Caption = 'g++: '
      end
      object lblmake: TLabel
        Left = 24
        Top = 160
        Width = 35
        Height = 15
        Caption = 'make: '
      end
      object lblgdb: TLabel
        Left = 24
        Top = 204
        Width = 27
        Height = 15
        Caption = 'gdb: '
      end
      object lblwindres: TLabel
        Left = 24
        Top = 248
        Width = 47
        Height = 15
        Caption = 'windres: '
      end
      object lblgprof: TLabel
        Left = 24
        Top = 288
        Width = 35
        Height = 15
        Caption = 'gprof: '
      end
      object btnBrowse2: TSpeedButton
        Tag = 2
        Left = 409
        Top = 67
        Width = 23
        Height = 22
        Anchors = [akTop, akRight]
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = btnBrws1Click
        ExplicitLeft = 419
      end
      object btnBrowse3: TSpeedButton
        Tag = 3
        Left = 409
        Top = 112
        Width = 23
        Height = 22
        Anchors = [akTop, akRight]
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = btnBrws1Click
        ExplicitLeft = 419
      end
      object btnBrowse4: TSpeedButton
        Tag = 4
        Left = 409
        Top = 156
        Width = 23
        Height = 22
        Anchors = [akTop, akRight]
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = btnBrws1Click
        ExplicitLeft = 419
      end
      object btnBrowse5: TSpeedButton
        Tag = 5
        Left = 409
        Top = 200
        Width = 23
        Height = 22
        Anchors = [akTop, akRight]
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = btnBrws1Click
        ExplicitLeft = 419
      end
      object btnBrowse6: TSpeedButton
        Tag = 6
        Left = 409
        Top = 243
        Width = 23
        Height = 22
        Anchors = [akTop, akRight]
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = btnBrws1Click
        ExplicitLeft = 419
      end
      object btnBrowse8: TSpeedButton
        Tag = 7
        Left = 409
        Top = 288
        Width = 23
        Height = 22
        Anchors = [akTop, akRight]
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = btnBrws1Click
        ExplicitLeft = 419
      end
      object GccEdit: TEdit
        Left = 80
        Top = 68
        Width = 319
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        OnChange = InterfaceChange
        ExplicitWidth = 329
      end
      object GppEdit: TEdit
        Left = 80
        Top = 112
        Width = 319
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
        OnChange = InterfaceChange
        ExplicitWidth = 329
      end
      object MakeEdit: TEdit
        Left = 80
        Top = 156
        Width = 319
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
        OnChange = InterfaceChange
        ExplicitWidth = 329
      end
      object GdbEdit: TEdit
        Left = 80
        Top = 200
        Width = 319
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 3
        OnChange = InterfaceChange
        ExplicitWidth = 329
      end
      object WindresEdit: TEdit
        Left = 80
        Top = 244
        Width = 319
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 4
        OnChange = InterfaceChange
        ExplicitWidth = 329
      end
      object GprofEdit: TEdit
        Left = 80
        Top = 288
        Width = 319
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 5
        OnChange = InterfaceChange
        ExplicitWidth = 329
      end
    end
  end
  object grpCompSet: TGroupBox
    Left = 12
    Top = 8
    Width = 461
    Height = 53
    Caption = 'Compiler set to configure'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 4
    object btnAddBlankCompilerSet: TSpeedButton
      Left = 360
      Top = 20
      Width = 22
      Height = 22
      Hint = 'Add a new compiler set'
      ImageIndex = 78
      ImageName = 'Plus'
      Images = dmMain.SVGImageListMenuStyle
      Flat = True
      OnClick = btnAddBlankCompilerSetClick
    end
    object btnDelCompilerSet: TSpeedButton
      Left = 432
      Top = 20
      Width = 22
      Height = 22
      Hint = 'Delete the selected compiler set'
      ImageIndex = 5
      ImageName = 'iconsnew-31'
      Images = dmMain.SVGImageListMenuStyle
      Flat = True
      OnClick = btnDelCompilerSetClick
    end
    object btnRenameCompilerSet: TSpeedButton
      Left = 408
      Top = 20
      Width = 22
      Height = 22
      Hint = 'Rename the selected compiler set'
      ImageIndex = 14
      ImageName = 'iconsnew-10'
      Images = dmMain.SVGImageListMenuStyle
      Flat = True
      OnClick = btnRenameCompilerSetClick
    end
    object btnFindCompilers: TSpeedButton
      Left = 336
      Top = 20
      Width = 22
      Height = 22
      Hint = 'Find and automatically configure compilers'
      ImageIndex = 41
      ImageName = 'iconsnew-49'
      Images = dmMain.SVGImageListMenuStyle
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnFindCompilersClick
    end
    object btnAddFilledCompilerSet: TSpeedButton
      Left = 384
      Top = 20
      Width = 22
      Height = 22
      Hint = 'Add a new compiler set'
      ImageIndex = 78
      ImageName = 'Plus'
      Images = dmMain.SVGImageListMenuStyle
      Flat = True
      OnClick = btnAddFilledCompilerSetClick
    end
    object cmbCompilerSetComp: TComboBox
      Left = 8
      Top = 20
      Width = 321
      Height = 23
      Style = csDropDownList
      TabOrder = 0
      OnChange = cmbCompilerSetCompChange
      OnEnter = cmbCompilerSetCompEnter
    end
  end
end
