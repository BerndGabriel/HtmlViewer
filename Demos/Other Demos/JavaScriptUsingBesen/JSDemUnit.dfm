object FormJSDemo: TFormJSDemo
  Left = 705
  Top = 161
  Caption = 'JavaScript Demo'
  ClientHeight = 828
  ClientWidth = 1171
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
    Width = 1171
    Height = 774
    CharSet = DEFAULT_CHARSET
    CodePage = 0
    DefBackground = clWindow
    DefFontName = 'Times New Roman'
    DefHotSpotColor = clNavy
    DefPreFontName = 'Courier New'
    fvOptions = [fvMetaRefresh, fvNoBorder, fvOverLinksActive, fvPrintTableBackground, fvPrintBackground, fvShowVScroll]
    HistoryIndex = 0
    HistoryMaxCount = 6
    ImageCacheCount = 6
    NoSelect = False
    PrintMarginBottom = 3.000000000000000000
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintScale = 1.000000000000000000
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
    OnScript = FrameViewerScript
    OnSoundRequest = SoundRequest
    Align = alClient
    TabOrder = 0
    OnMouseMove = FrameViewerMouseMove
    Touch.InteractiveGestures = [igPan]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia]
    OnFormSubmit = SubmitEvent
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1171
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 1
    DesignSize = (
      1171
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
      Left = 200
      Top = 2
      Width = 985
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
    Top = 801
    Width = 1171
    Height = 27
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 3
    object ProgressBar: TProgressBar
      Left = 1019
      Top = 2
      Width = 150
      Height = 23
      Align = alRight
      TabOrder = 0
      Visible = False
    end
    object InfoPanel: TPanel
      Left = 89
      Top = 2
      Width = 930
      Height = 23
      Align = alClient
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      TabOrder = 1
    end
    object QuirksModePanel: TPanel
      Left = 2
      Top = 2
      Width = 87
      Height = 23
      Align = alLeft
      BevelOuter = bvLowered
      TabOrder = 2
    end
  end
  object MainMenu: TMainMenu
    Left = 88
    Top = 40
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
      object mmiQuirksMode: TMenuItem
        Caption = 'Quirks Mode'
        object mmiQuirksModeStandards: TMenuItem
          Caption = 'Standards'
          Hint = 'Closest to what HTML and CSS are supposed to do'
          RadioItem = True
          OnClick = mmiQuirksModeStandardsClick
        end
        object mmiQuirksModeDetect: TMenuItem
          Caption = 'Detect'
          Hint = 
            'Emulate quirks, if document is not designed for HTML 4.01 or lat' +
            'er'
          RadioItem = True
          OnClick = mmiQuirksModeDetectClick
        end
        object mmiQuirksModeQuirks: TMenuItem
          Caption = 'Quirks'
          Hint = 'Emulates quirks of legacy browsers built before HTML 4.01'
          RadioItem = True
          OnClick = mmiQuirksModeQuirksClick
        end
      end
      object ShowLog: TMenuItem
        Caption = 'Show Log'
        OnClick = ShowLogClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object LogDiag: TMenuItem
        Caption = 'Log Diagnostics'
        OnClick = LogDiagClick
      end
      object LogScript: TMenuItem
        Caption = 'Log JS Script'
        OnClick = LogScriptClick
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
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 304
    Top = 40
  end
  object FindDialog: TFindDialog
    Options = [frDown, frHideWholeWord, frDisableWholeWord]
    OnFind = FindDialogFind
    Left = 368
    Top = 40
  end
  object PrintDialog: TPrintDialog
    FromPage = 1
    MinPage = 1
    MaxPage = -1
    Options = [poPageNums]
    ToPage = 1
    Left = 544
    Top = 41
  end
  object PopupMenu: TPopupMenu
    Left = 152
    Top = 41
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
    Left = 220
    Top = 42
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 440
    Top = 41
  end
end
