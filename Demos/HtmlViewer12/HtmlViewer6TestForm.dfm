object FormHtmlViewer12Test: TFormHtmlViewer12Test
  Left = 648
  Top = 25
  Width = 1091
  Height = 707
  Caption = 'HtmlViewer 12 Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = menu
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BegaSplitter1: TSplitter
    Left = 395
    Top = 0
    Width = 8
    Height = 653
    Cursor = crHSplit
    ResizeStyle = rsUpdate
  end
  object HtmlViewer: THtmlViewer12
    Left = 403
    Top = 0
    Width = 680
    Height = 653
    Align = alClient
    Color = clTeal
    DoubleBuffered = False
    ParentColor = False
    TabOrder = 1
    ViewerOptions = []
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 395
    Height = 653
    ActivePage = CssTab
    Align = alLeft
    TabIndex = 1
    TabOrder = 0
    object HtmlTab: TTabSheet
      Caption = 'Html'
    end
    object CssTab: TTabSheet
      Caption = 'Css'
      ImageIndex = 1
      object CssMemo: TMemo
        Left = 0
        Top = 0
        Width = 387
        Height = 625
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object menu: TMainMenu
    Left = 272
    Top = 64
    object menuFile: TMenuItem
      Caption = 'File'
      object menuFileOpen: TMenuItem
        Caption = 'Open...'
        OnClick = menuFileOpenClick
      end
    end
  end
  object dlgFileOpen: TTntOpenDialog
    Left = 128
    Top = 80
  end
end
