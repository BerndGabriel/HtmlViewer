object FormHtmlViewer12Test: TFormHtmlViewer12Test
  Left = 0
  Top = 0
  Caption = 'HtmlViewer 12 Test'
  ClientHeight = 361
  ClientWidth = 1083
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = menu
  OldCreateOrder = False
  ScreenSnap = True
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BegaSplitter1: TBegaSplitter
    Left = 433
    Top = 21
    Width = 8
    Height = 340
    Cursor = crHSplit
    ButtonAlign = baCenter
    ButtonKind = bkNone
    ResizeStyle = rsUpdate
    ExplicitLeft = 633
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
  object vtDocument: TBegaVirtualStringTree
    Left = 0
    Top = 21
    Width = 433
    Height = 340
    Align = alLeft
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible, hoAutoSpring]
    NodeDataSize = 4
    TabOrder = 1
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
    OnGetText = vtDocumentGetText
    OnInitChildren = vtDocumentInitChildren
    OnInitNode = vtDocumentInitNode
    Columns = <
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
        Position = 0
        Width = 61
        WideText = 'Element'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
        Position = 1
        Width = 61
        WideText = 'Class'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
        Position = 2
        Width = 61
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
        Width = 61
        WideText = 'Style'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
        Position = 5
        Width = 61
        WideText = 'Props'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus]
        Position = 6
        Width = 64
        WideText = 'Attrs'
      end>
  end
  object HtmlViewer: THtmlViewer12
    Left = 441
    Top = 21
    Width = 642
    Height = 340
    Align = alClient
    Color = clBtnFace
    DoubleBuffered = True
    ParentColor = False
    TabOrder = 2
    ViewerOptions = []
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
