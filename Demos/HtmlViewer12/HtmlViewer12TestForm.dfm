object FormHtmlViewer12Test: TFormHtmlViewer12Test
  Left = 0
  Top = 0
  Caption = 'HtmlViewer 12 Test'
  ClientHeight = 921
  ClientWidth = 1408
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
    Left = 600
    Top = 21
    Width = 8
    Height = 900
    ResizeStyle = rsUpdate
    ExplicitLeft = 395
    ExplicitHeight = 632
  end
  object cbFiles: TBegaCombobox
    Left = 0
    Top = 0
    Width = 1408
    Height = 21
    Align = alTop
    DropDownCount = 32
    ItemHeight = 13
    TabOrder = 0
    OnKeyPress = cbFilesKeyPress
    RegistryKey = 'Software\Fast Function Factory\BegaViewer\BrowseURL'
    RegistryValuePrefix = 'URL'
    ExplicitWidth = 1083
  end
  object HtmlViewer: THtmlViewer12
    Left = 608
    Top = 21
    Width = 800
    Height = 900
    Align = alClient
    Color = clBtnFace
    DoubleBuffered = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 1
    ViewerOptions = []
    ExplicitLeft = 403
    ExplicitWidth = 680
    ExplicitHeight = 672
  end
  object PageControl: TPageControl
    Left = 0
    Top = 21
    Width = 600
    Height = 900
    ActivePage = HtmlTab
    Align = alLeft
    TabOrder = 2
    ExplicitHeight = 672
    object HtmlTab: TTabSheet
      Caption = 'Html'
      ExplicitWidth = 387
      ExplicitHeight = 644
      object vtDocument: TBegaVirtualStringTree
        Left = 0
        Top = 0
        Width = 592
        Height = 872
        Align = alClient
        Header.AutoSizeIndex = -1
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible, hoAutoSpring]
        NodeDataSize = 4
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
        OnGetText = vtDocumentGetText
        OnInitChildren = vtDocumentInitChildren
        OnInitNode = vtDocumentInitNode
        ExplicitWidth = 387
        ExplicitHeight = 644
        Columns = <
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
            Position = 0
            Width = 234
            WideText = 'Element'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
            Position = 1
            Width = 125
            WideText = 'Class'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
            Position = 2
            Width = 125
            WideText = 'Id'
          end
          item
            Position = 3
            WideText = 'Display'
          end
          item
            Position = 4
            Width = 60
            WideText = 'DocPos'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
            Position = 5
            Width = 125
            WideText = 'Style'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
            Position = 6
            Width = 125
            WideText = 'Props'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
            Position = 7
            Width = 10
            WideText = 'Attrs'
          end>
      end
    end
    object CssTab: TTabSheet
      Caption = 'Css'
      ImageIndex = 1
      ExplicitWidth = 387
      ExplicitHeight = 644
      object CssMemo: TMemo
        Left = 0
        Top = 0
        Width = 592
        Height = 872
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        ExplicitWidth = 387
        ExplicitHeight = 644
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
      object menuFileReload: TMenuItem
        Caption = 'Reload'
        ShortCut = 116
        OnClick = menuFileReloadClick
      end
    end
  end
  object dlgFileOpen: TTntOpenDialog
    Left = 128
    Top = 80
  end
end
