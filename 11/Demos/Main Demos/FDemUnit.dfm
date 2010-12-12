object Form1: TForm1
  Left = 135
  Top = 398
  Caption = 'Frame Demo'
  ClientHeight = 292
  ClientWidth = 494
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Panel2: TPanel
    Left = 0
    Top = 269
    Width = 494
    Height = 23
    Align = alBottom
    Alignment = taLeftJustify
    BevelInner = bvLowered
    TabOrder = 0
    object ProgressBar: TProgressBar
      Left = 342
      Top = 2
      Width = 150
      Height = 19
      Align = alRight
      TabOrder = 0
      Visible = False
    end
  end
  object FrameViewer: TFrameViewer
    Left = 0
    Top = 25
    Width = 494
    Height = 244
    CharSet = DEFAULT_CHARSET
    DefBackground = clWindow
    DefFontName = 'Times New Roman'
    DefHotSpotColor = clNavy
    DefPreFontName = 'Courier New'
    fvOptions = [fvMetaRefresh, fvNoBorder, fvOverLinksActive, fvPrintMonochromeBlack, fvPrintTableBackground, fvShowVScroll]
    HistoryIndex = 0
    HistoryMaxCount = 6
    ImageCacheCount = 6
    NoSelect = False
    OnBlankWindowRequest = WindowRequest
    OnHistoryChange = HistoryChange
    OnHotSpotTargetClick = HotSpotTargetClick
    OnHotSpotTargetCovered = HotspotTargetChange
    OnInclude = FrameViewerInclude
    OnObjectClick = FrameViewerObjectClick
    OnPrintHTMLFooter = ViewerPrintHTMLFooter
    OnPrintHTMLHeader = ViewerPrintHTMLHeader
    OnProcessing = ProcessingHandler
    OnProgress = FrameViewerProgress
    OnRightClick = FrameViewerRightClick
    OnSoundRequest = SoundRequest
    PrintMarginBottom = 3.000000000000000000
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintScale = 1.000000000000000000
    Align = alClient
    TabOrder = 1
    OnMouseMove = FrameViewerMouseMove
    OnFormSubmit = SubmitEvent
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 494
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object ReloadButton: TButton
      Left = 8
      Top = 0
      Width = 65
      Height = 25
      Caption = '&Reload'
      Enabled = False
      TabOrder = 0
      OnClick = ReloadClick
    end
    object FwdButton: TButton
      Left = 72
      Top = 0
      Width = 65
      Height = 25
      Caption = '&Fwd'
      Enabled = False
      TabOrder = 1
      OnClick = FwdButtonClick
    end
    object BackButton: TButton
      Left = 136
      Top = 0
      Width = 65
      Height = 25
      Caption = '&Back'
      Enabled = False
      TabOrder = 2
      OnClick = BackButtonClick
    end
    object Edit2: TEdit
      Left = 208
      Top = 0
      Width = 337
      Height = 24
      TabOrder = 3
    end
  end
  object MediaPlayer: TMediaPlayer
    Left = 76
    Top = 115
    Width = 253
    Height = 30
    Visible = False
    TabOrder = 3
    OnNotify = MediaPlayerNotify
  end
  object MainMenu1: TMainMenu
    Left = 360
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      OnClick = File1Click
      object Open1: TMenuItem
        Caption = '&Open'
        ShortCut = 114
        OnClick = Open1Click
      end
      object SetPrintScale: TMenuItem
        Caption = 'Set PrintScale'
        OnClick = SetPrintScaleClick
      end
      object PrinterSetup: TMenuItem
        Caption = 'Printer Setup...'
        OnClick = PrinterSetupClick
      end
      object PrintPreview1: TMenuItem
        Caption = 'Print Pre&view'
        Enabled = False
        OnClick = PrintPreview1Click
      end
      object Print1: TMenuItem
        Caption = '&Print...'
        OnClick = Print1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = '&Exit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      OnClick = Edit1Click
      object Find1: TMenuItem
        Caption = '&Find'
        ShortCut = 16454
        OnClick = Find1Click
      end
      object Copy1: TMenuItem
        Caption = '&Copy'
        ShortCut = 16451
        OnClick = Copy1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object SelectAll1: TMenuItem
        Caption = '&Select All'
        OnClick = SelectAll1Click
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object Showimages: TMenuItem
        Caption = '&Show images'
        Checked = True
        OnClick = ShowimagesClick
      end
      object Fonts: TMenuItem
        Caption = 'Default &Font/Colors'
        OnClick = FontsClick
      end
    end
    object HistoryMenuItem: TMenuItem
      Caption = '&History'
      Visible = False
    end
    object About1: TMenuItem
      Caption = '&About'
      OnClick = About1Click
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'htm'
    Filter = 'html files|*.htm;*.html|all files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Left = 320
    Top = 72
  end
  object FindDialog: TFindDialog
    Options = [frDown, frHideWholeWord, frDisableWholeWord]
    OnFind = FindDialogFind
    Left = 400
    Top = 72
  end
  object PrintDialog: TPrintDialog
    FromPage = 1
    MinPage = 1
    MaxPage = -1
    Options = [poPageNums]
    ToPage = 1
    Left = 272
    Top = 73
  end
  object PopupMenu: TPopupMenu
    Left = 360
    Top = 105
    object ViewImage: TMenuItem
      Caption = '&View Image'
      OnClick = ViewImageClick
    end
    object CopyImagetoclipboard: TMenuItem
      Caption = '&Copy image to clipboard'
      OnClick = CopyImagetoclipboardClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object OpenInNewWindow: TMenuItem
      Caption = '&Open in new window'
      OnClick = OpenInNewWindowClick
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 200
    OnTimer = Timer1Timer
    Left = 236
    Top = 66
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 440
    Top = 65
  end
end
