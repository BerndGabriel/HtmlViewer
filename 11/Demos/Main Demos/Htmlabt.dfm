object AboutBox: TAboutBox
  Left = 286
  Top = 214
  Width = 375
  Height = 265
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
    231)
  PixelsPerInch = 96
  TextHeight = 16
  object BitBtn1: TBitBtn
    Left = 284
    Top = 198
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 0
    Kind = bkOK
  end
  object Viewer: THTMLViewer
    Left = 8
    Top = 8
    Width = 352
    Height = 180
    ViewImages = False
    Enabled = False
    TabOrder = 1
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = htNone
    HistoryMaxCount = 0
    DefFontName = 'Times New Roman'
    DefPreFontName = 'Courier New'
    NoSelect = True
    ScrollBars = ssNone
    CharSet = DEFAULT_CHARSET
    PrintMarginLeft = 2
    PrintMarginRight = 2
    PrintMarginTop = 2
    PrintMarginBottom = 2
    PrintScale = 1
    htOptions = []
  end
end
