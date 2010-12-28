object Form1: TForm1
  Left = 192
  Top = 116
  Width = 638
  Height = 477
  Caption = 'Form1'
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 630
    Height = 41
    Align = alTop
    TabOrder = 0
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
  object Viewer: THTMLViewer
    Left = 0
    Top = 41
    Width = 630
    Height = 409
    TabOrder = 1
    Align = alClient
    DefBackground = clWindow
    BorderStyle = htFocused
    HistoryMaxCount = 0
    DefFontName = 'Times New Roman'
    DefPreFontName = 'Courier New'
    NoSelect = False
    CharSet = DEFAULT_CHARSET
    PrintMarginLeft = 2
    PrintMarginRight = 2
    PrintMarginTop = 2
    PrintMarginBottom = 2
    htOptions = [htPrintTableBackground, htPrintMonochromeBlack, htShowVScroll]
    OnPanelCreate = ViewerPanelCreate
    OnPanelDestroy = ViewerPanelDestroy
    object MediaPlayer: TMediaPlayer
      Left = 304
      Top = 160
      Width = 253
      Height = 30
      Visible = False
      TabOrder = 4
    end
  end
end
