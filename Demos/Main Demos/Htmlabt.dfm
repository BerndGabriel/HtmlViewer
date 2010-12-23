object AboutBox: TAboutBox
  Left = 740
  Top = 213
  Width = 375
  Height = 291
  BorderIcons = [biSystemMenu]
  Caption = 'About'
  Color = clBtnFace
  Constraints.MinHeight = 265
  Constraints.MinWidth = 375
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    367
    257)
  PixelsPerInch = 96
  TextHeight = 16
  object BitBtn1: TBitBtn
    Left = 284
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 0
    Kind = bkOK
  end
  object Viewer: THtmlViewer
    Left = 8
    Top = 8
    Width = 352
    Height = 206
    Enabled = False
    TabOrder = 1
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = htNone
    CharSet = DEFAULT_CHARSET
    DefFontName = 'Times New Roman'
    DefPreFontName = 'Courier New'
    HistoryMaxCount = 0
    HtOptions = []
    NoSelect = True
    PrintMarginBottom = 2
    PrintMarginLeft = 2
    PrintMarginRight = 2
    PrintMarginTop = 2
    PrintScale = 1
    ScrollBars = ssNone
    ViewImages = False
  end
end
