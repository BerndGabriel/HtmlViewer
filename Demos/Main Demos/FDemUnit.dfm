object Form1: TForm1
  Left = 705
  Top = 161
  Caption = 'Frame Demo'
  ClientHeight = 714
  ClientWidth = 1155
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object FrameViewer: TFrameViewer
    Left = 0
    Top = 27
    Width = 1155
    Height = 660
    CodePage = 0
    DefBackground = clWindow
    DefFontColor = clWindowText
    DefHotSpotColor = clNavy
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
    Text = 'Hello Serif<pre>Hello Monospace</pre>'
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
    Align = alClient
    TabOrder = 0
    OnMouseMove = FrameViewerMouseMove
    OnFormSubmit = SubmitEvent
    ExplicitWidth = 1151
    ExplicitHeight = 659
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1155
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 1
    ExplicitWidth = 1151
    DesignSize = (
      1155
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
      Width = 939
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 3
      ExplicitWidth = 935
    end
  end
  object MediaPlayer: TMediaPlayer
    Left = 76
    Top = 115
    Width = 253
    Height = 30
    DoubleBuffered = True
    Visible = False
    ParentDoubleBuffered = False
    TabOrder = 2
    OnNotify = MediaPlayerNotify
  end
  object Panel3: TPanel
    Left = 0
    Top = 687
    Width = 1155
    Height = 27
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 3
    ExplicitTop = 686
    ExplicitWidth = 1151
    object ProgressBar: TProgressBar
      Left = 1003
      Top = 2
      Width = 150
      Height = 23
      Align = alRight
      TabOrder = 0
      Visible = False
      ExplicitLeft = 999
    end
    object InfoPanel: TPanel
      Left = 89
      Top = 2
      Width = 914
      Height = 23
      Align = alClient
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      TabOrder = 1
      ExplicitWidth = 910
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
    Left = 360
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
      object mmiDefaultFont: TMenuItem
        Caption = 'Default &Font/Colors'
        OnClick = mmiDefaultFontClick
      end
      object mmiParentFont: TMenuItem
        Caption = 'Parent Font'
        OnClick = mmiParentFontClick
      end
      object mmiParentColor: TMenuItem
        Caption = 'Parent Color'
        OnClick = mmiParentColorClick
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
