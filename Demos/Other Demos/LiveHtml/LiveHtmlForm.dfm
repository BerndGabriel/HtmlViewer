object FormLiveHtml: TFormLiveHtml
  Left = 0
  Top = 0
  Caption = 'Live Html'
  ClientHeight = 466
  ClientWidth = 753
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 263
    Width = 753
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 265
    ExplicitWidth = 635
  end
  object HtmlViewer: THtmlViewer
    Left = 0
    Top = 0
    Width = 753
    Height = 263
    TabOrder = 0
    Align = alClient
    PopupMenu = PopupMenu
    BorderStyle = htFocused
    DefBackground = clCream
    DefFontName = 'Times New Roman'
    DefPreFontName = 'Courier New'
    HistoryMaxCount = 0
    NoSelect = False
    PrintMarginBottom = 2.000000000000000000
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintScale = 1.000000000000000000
    QuirksMode = qmDetect
    ScrollBars = ssNone
    OnObjectClick = HtmlViewerObjectClick
    OnParseBegin = HtmlViewerParseBegin
  end
  object Memo: TSynEdit
    Left = 0
    Top = 266
    Width = 753
    Height = 200
    Cursor = crDefault
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    PopupMenu = PopupMenu1
    TabOrder = 1
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    HideSelection = True
    Highlighter = SynHTMLSyn1
    Options = [eoAutoIndent, eoDragDropEditing, eoDropFiles, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
    RightEdge = 100
    OnChange = MemoChange
  end
  object PopupMenu: TPopupMenu
    Left = 368
    Top = 64
    object pmSelectAll: TMenuItem
      Caption = 'Select All'
      OnClick = pmSelectAllClick
    end
    object pmCopy: TMenuItem
      Caption = 'Copy'
      OnClick = pmCopyClick
    end
    object pmPaste: TMenuItem
      Caption = 'Paste'
      OnClick = pmPasteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object NameList1: TMenuItem
      Caption = 'NameList'
      OnClick = NameList1Click
    end
    object DocumentSource1: TMenuItem
      Caption = 'DocumentSource'
      OnClick = DocumentSource1Click
    end
    object TestAsString1: TMenuItem
      AutoCheck = True
      Caption = 'Test AsString'
    end
  end
  object SynHTMLSyn1: TSynHTMLSyn
    Left = 88
    Top = 360
  end
  object SynEditOptionsDialog1: TSynEditOptionsDialog
    UseExtendedStrings = False
    Left = 136
    Top = 360
  end
  object PopupMenu1: TPopupMenu
    Left = 184
    Top = 360
    object Options1: TMenuItem
      Caption = 'Options'
      OnClick = Options1Click
    end
  end
end
