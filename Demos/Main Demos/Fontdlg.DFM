object FontForm: TFontForm
  Left = 622
  Top = 349
  Width = 498
  Height = 456
  BorderIcons = [biSystemMenu]
  Caption = 'Edit Default Font and Colors'
  Color = clBtnFace
  Constraints.MinHeight = 455
  Constraints.MinWidth = 355
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    482
    418)
  PixelsPerInch = 96
  TextHeight = 16
  object FontColorLabel: TLabel
    Left = 8
    Top = 325
    Width = 61
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = '&Font Color'
  end
  object LinkColorLabel: TLabel
    Left = 8
    Top = 357
    Width = 59
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = '&Link Color'
  end
  object FontSizeLabel: TLabel
    Left = 8
    Top = 296
    Width = 55
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Font &Size'
    FocusControl = FontSizeEdit
  end
  object FontNameLabel: TLabel
    Left = 8
    Top = 268
    Width = 66
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Font &Name'
  end
  object BackgroundColorLabel: TLabel
    Left = 8
    Top = 387
    Width = 73
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = '&Background'
  end
  object FontColorButton: TSpeedButton
    Left = 235
    Top = 321
    Width = 23
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = '...'
    OnClick = FontColorButtonClick
  end
  object LinkColorButton: TSpeedButton
    Left = 235
    Top = 353
    Width = 23
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = '...'
    OnClick = LinkColorButtonClick
  end
  object BackgroundColorButton: TSpeedButton
    Left = 235
    Top = 385
    Width = 23
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = '...'
    OnClick = BackgroundColorButtonClick
  end
  object OKButton: TButton
    Left = 399
    Top = 353
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'O&K'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Cancel: TButton
    Left = 399
    Top = 385
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ResetButton: TButton
    Left = 399
    Top = 321
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Reset'
    TabOrder = 2
    OnClick = ResetButtonClick
  end
  object FontSizeEdit: TSpinEdit
    Left = 88
    Top = 292
    Width = 65
    Height = 26
    Anchors = [akLeft, akBottom]
    MaxValue = 24
    MinValue = 6
    TabOrder = 0
    Value = 12
    OnChange = AnythingChanged
  end
  object FontViewer: THtmlViewer
    Left = 8
    Top = 8
    Width = 466
    Height = 249
    BorderStyle = htSingle
    DefFontName = 'Times New Roman'
    DefPreFontName = 'Courier New'
    HistoryMaxCount = 0
    HtOptions = []
    NoSelect = True
    PrintMarginBottom = 2
    PrintMarginLeft = 2
    PrintMarginRight = 2
    PrintMarginTop = 2
    PrintScale = 1
    ScrollBars = ssVertical
    ViewImages = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
  end
  object FontNameComboBox: TComboBox
    Left = 88
    Top = 265
    Width = 386
    Height = 24
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 16
    TabOrder = 5
    OnClick = AnythingChanged
  end
  object FontColorComboBox: TComboBox
    Left = 88
    Top = 321
    Width = 145
    Height = 24
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    DropDownCount = 32
    ItemHeight = 16
    TabOrder = 6
    OnChange = AnythingChanged
  end
  object LinkColorComboBox: TComboBox
    Left = 88
    Top = 353
    Width = 145
    Height = 24
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    DropDownCount = 32
    ItemHeight = 16
    TabOrder = 7
    OnChange = AnythingChanged
  end
  object BackgroundColorComboBox: TComboBox
    Left = 88
    Top = 385
    Width = 145
    Height = 24
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    DropDownCount = 32
    ItemHeight = 16
    TabOrder = 8
    OnChange = AnythingChanged
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Left = 136
    Top = 176
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 176
    Top = 176
  end
end
