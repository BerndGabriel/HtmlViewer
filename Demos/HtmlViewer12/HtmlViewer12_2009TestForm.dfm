object FormHtmlViewer12Test: TFormHtmlViewer12Test
  Left = 0
  Top = 0
  Caption = 'HtmlViewer 12 Test'
  ClientHeight = 653
  ClientWidth = 1083
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
    Top = 21
    Width = 8
    Height = 632
    ResizeStyle = rsUpdate
  end
  object cbFiles: TBegaCombobox
    Left = 0
    Top = 0
    Width = 1083
    Height = 21
    Align = alTop
    DropDownCount = 32
    ItemHeight = 13
    TabOrder = 0
    OnKeyPress = cbFilesKeyPress
    RegistryKey = 'Software\Fast Function Factory\BegaViewer\BrowseURL'
    RegistryValuePrefix = 'URL'
  end
  object HtmlViewer: THtmlViewer12
    Left = 403
    Top = 21
    Width = 680
    Height = 632
    Align = alClient
    Color = clBtnFace
    ParentColor = False
    TabOrder = 1
    ViewerOptions = []
  end
  object PageControl: TPageControl
    Left = 0
    Top = 21
    Width = 395
    Height = 632
    ActivePage = CssTab
    Align = alLeft
    TabOrder = 2
    object HtmlTab: TTabSheet
      Caption = 'Html'
      object vtDocument: TBegaVirtualStringTree
        Left = 0
        Top = 0
        Width = 387
        Height = 604
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
        Columns = <
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
            Position = 0
            Width = 91
            WideText = 'Element'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
            Position = 1
            Width = 91
            WideText = 'Class'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
            Position = 2
            Width = 91
            WideText = 'Id'
          end
          item
            Position = 3
            Width = 60
            WideText = 'DocPos'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
            Position = 4
            Width = 91
            WideText = 'Style'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
            Position = 5
            Width = 91
            WideText = 'Props'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
            Position = 6
            Width = 10
            WideText = 'Attrs'
          end>
      end
    end
    object CssTab: TTabSheet
      Caption = 'Css'
      ImageIndex = 1
      object CssMemo: TMemo
        Left = 0
        Top = 0
        Width = 387
        Height = 604
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
        ShortCut = 16463
        OnClick = menuFileOpenClick
      end
      object menuFileRefresh: TMenuItem
        Caption = 'Refresh'
        ShortCut = 116
        OnClick = menuFileRefreshClick
      end
    end
  end
  object dlgFileOpen: TOpenDialog
    Left = 128
    Top = 80
  end
end
