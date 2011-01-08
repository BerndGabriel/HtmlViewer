object Form1: TForm1
  Left = 496
  Top = 218
  Width = 708
  Height = 475
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 700
    Height = 41
    Align = alTop
    TabOrder = 0
    object Button1: TButton
      Tag = 3
      Left = 248
      Top = 8
      Width = 25
      Height = 25
      Caption = 'B'
      TabOrder = 0
      OnClick = ButtonClick
    end
    object Button2: TButton
      Tag = 1
      Left = 272
      Top = 8
      Width = 25
      Height = 25
      Caption = 'I'
      TabOrder = 1
      OnClick = ButtonClick
    end
    object Button3: TButton
      Tag = 2
      Left = 296
      Top = 8
      Width = 25
      Height = 25
      Caption = 'U'
      TabOrder = 2
      OnClick = ButtonClick
    end
  end
  object Panel: TPanel
    Left = 0
    Top = 41
    Width = 700
    Height = 361
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter: TSplitter
      Left = 0
      Top = 209
      Width = 700
      Height = 5
      Cursor = crVSplit
      Align = alTop
      OnMoved = SplitterMoved
    end
    object RichEdit: TRichEdit
      Left = 0
      Top = 0
      Width = 700
      Height = 209
      Align = alTop
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      PlainText = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      OnChange = RichEdChange
      OnSelectionChange = RichEditSelectionChange
    end
    object Panel2: TPanel
      Left = 0
      Top = 214
      Width = 700
      Height = 147
      Align = alClient
      BevelOuter = bvLowered
      BevelWidth = 2
      Caption = 'Panel2'
      Color = clWindow
      TabOrder = 1
      object Viewer: THtmlViewer
        Left = 2
        Top = 2
        Width = 696
        Height = 143
        TabOrder = 0
        Align = alClient
        BorderStyle = htFocused
        CharSet = DEFAULT_CHARSET
        DefBackground = clWindow
        DefFontName = 'Times New Roman'
        DefPreFontName = 'Courier New'
        HistoryMaxCount = 0
        HtOptions = [htShowDummyCaret]
        NoSelect = False
        PrintMarginBottom = 2
        PrintMarginLeft = 2
        PrintMarginRight = 2
        PrintMarginTop = 2
        PrintScale = 1
        OnMouseUp = ViewerMouseUp
        OnHotSpotClick = ViewerHotSpotClick
        OnHotSpotCovered = ViewerHotSpotCovered
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 402
    Width = 700
    Height = 19
    Align = alBottom
    Alignment = taLeftJustify
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 2
  end
  object MainMenu1: TMainMenu
    Left = 400
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New'
        OnClick = New1Click
      end
      object NewHTML: TMenuItem
        Caption = 'New &HTML'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = '&Open'
        ShortCut = 114
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = '&Save'
        ShortCut = 113
        OnClick = Save1Click
      end
      object Saveas1: TMenuItem
        Caption = 'Save &as'
        OnClick = Saveas1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object ExitItem: TMenuItem
        Caption = 'E&xit'
        OnClick = ExitItemClick
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Copy1: TMenuItem
        Caption = '&Copy'
        ShortCut = 16429
        OnClick = EditCopy
      end
      object Cut1: TMenuItem
        Caption = 'Cu&t'
        ShortCut = 8238
        OnClick = EditCut
      end
      object Paste1: TMenuItem
        Caption = '&Paste'
        ShortCut = 8237
        OnClick = EditPaste
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'htm'
    Filter = 'HTML Files|*.htm; *.html|All Files|*.*'
    Left = 440
  end
  object SaveDialog1: TSaveDialog
    Left = 472
  end
end
