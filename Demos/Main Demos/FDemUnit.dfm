object Form1: TForm1
  Left = 705
  Top = 161
  Width = 1195
  Height = 894
  Caption = 'Frame Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  Position = poDesktopCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object FrameViewer: TFrameViewer
    Left = 0
    Top = 27
    Width = 1179
    Height = 782
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
    OnHotSpotTargetCovered = HotSpotTargetCovered
    OnInclude = FrameViewerInclude
    OnObjectClick = FrameViewerObjectClick
    OnPrintHTMLFooter = ViewerPrintHTMLFooter
    OnPrintHTMLHeader = ViewerPrintHTMLHeader
    OnProcessing = ProcessingHandler
    OnProgress = FrameViewerProgress
    OnRightClick = FrameViewerRightClick
    OnSoundRequest = SoundRequest
    PrintMarginBottom = 3
    PrintMarginLeft = 2
    PrintMarginRight = 2
    PrintMarginTop = 2
    PrintScale = 1
    Align = alClient
    TabOrder = 0
    OnMouseMove = FrameViewerMouseMove
    OnFormSubmit = SubmitEvent
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1179
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 1
    DesignSize = (
      1179
      27)
    object ReloadButton: TButton
      Left = 2
      Top = 2
      Width = 65
      Height = 23
      Anchors = [akLeft, akTop, akBottom]
      Caption = '&Reload'
      Enabled = False
      TabOrder = 0
      OnClick = ReloadClick
    end
    object FwdButton: TButton
      Left = 67
      Top = 2
      Width = 65
      Height = 23
      Anchors = [akLeft, akTop, akBottom]
      Caption = '&Fwd'
      Enabled = False
      TabOrder = 1
      OnClick = FwdButtonClick
    end
    object BackButton: TButton
      Left = 132
      Top = 2
      Width = 65
      Height = 23
      Anchors = [akLeft, akTop, akBottom]
      Caption = '&Back'
      Enabled = False
      TabOrder = 2
      OnClick = BackButtonClick
    end
    object Edit2: TEdit
      Left = 197
      Top = 2
      Width = 988
      Height = 24
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 3
    end
  end
  object MediaPlayer: TMediaPlayer
    Left = 76
    Top = 115
    Width = 253
    Height = 30
    Visible = False
    TabOrder = 2
    OnNotify = MediaPlayerNotify
  end
  object Panel3: TPanel
    Left = 0
    Top = 809
    Width = 1179
    Height = 27
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 3
    object ProgressBar: TProgressBar
      Left = 1027
      Top = 2
      Width = 150
      Height = 23
      Align = alRight
      Min = 0
      Max = 100
      TabOrder = 0
      Visible = False
    end
    object InfoPanel: TPanel
      Left = 2
      Top = 2
      Width = 1025
      Height = 23
      Align = alClient
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      TabOrder = 1
    end
  end
  object MainMenu: TMainMenu
    Left = 360
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      OnClick = File1Click
      object Open: TMenuItem
        Caption = '&Open'
        ShortCut = 114
        OnClick = OpenClick
      end
      object SetPrintScale: TMenuItem
        Caption = 'Set PrintScale'
        OnClick = SetPrintScaleClick
      end
      object PrinterSetup: TMenuItem
        Caption = 'Printer Setup...'
        OnClick = PrinterSetupClick
      end
      object PrintPreview: TMenuItem
        Caption = 'Print Pre&view...'
        Enabled = False
        OnClick = PrintPreviewClick
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
      object CopyItem: TMenuItem
        Caption = '&Copy'
        ShortCut = 16451
        OnClick = CopyItemClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object SelectAllItem: TMenuItem
        Caption = '&Select All'
        OnClick = SelectAllItemClick
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
