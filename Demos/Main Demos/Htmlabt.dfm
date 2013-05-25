object AboutBox: TAboutBox
  Left = 740
  Top = 213
  BorderIcons = [biSystemMenu]
  Caption = 'About'
  ClientHeight = 253
  ClientWidth = 359
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
    359
    253)
  PixelsPerInch = 96
  TextHeight = 16
  object BitBtn1: TBitBtn
    Left = 276
    Top = 220
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    TabOrder = 0
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
    DefFontName = 'Times New Roman'
    DefPreFontName = 'Courier New'
    HistoryMaxCount = 0
    HtOptions = []
    NoSelect = True
    PrintMarginBottom = 2.000000000000000000
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintScale = 1.000000000000000000
    ScrollBars = ssNone
    ViewImages = False
  end
end
