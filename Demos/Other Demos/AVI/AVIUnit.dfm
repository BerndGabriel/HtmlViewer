object Form1: TForm1
  Left = 192
  Top = 116
  Caption = 'Form1'
  ClientHeight = 439
  ClientWidth = 626
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 626
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 622
    object LoadButton: TButton
      Left = 16
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Load AviPanel.htm'
      TabOrder = 0
      OnClick = LoadButtonClick
    end
    object PlayButton: TButton
      Left = 152
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Play'
      Enabled = False
      TabOrder = 1
      OnClick = PlayButtonClick
    end
  end
  object Viewer: THtmlViewer
    Left = 0
    Top = 41
    Width = 626
    Height = 398
    BorderStyle = htFocused
    DefBackground = clWindow
    DefFontName = 'Times New Roman'
    DefPreFontName = 'Courier New'
    HistoryMaxCount = 0
    HtOptions = [htPrintTableBackground, htPrintMonochromeBlack, htShowVScroll]
    NoSelect = False
    PrintMarginBottom = 2.000000000000000000
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintScale = 1.000000000000000000
    Text = ''
    OnPanelCreate = ViewerPanelCreate
    OnPanelDestroy = ViewerPanelDestroy
    Align = alClient
    TabOrder = 1
    Touch.InteractiveGestures = [igPan]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia]
    ExplicitWidth = 622
    ExplicitHeight = 397
    object MediaPlayer: TMediaPlayer
      Left = 304
      Top = 160
      Width = 253
      Height = 30
      DoubleBuffered = True
      Visible = False
      ParentDoubleBuffered = False
      TabOrder = 1
    end
  end
end
