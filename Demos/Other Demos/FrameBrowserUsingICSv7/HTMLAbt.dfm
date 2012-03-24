object AboutBox: TAboutBox
  Left = 245
  Top = 137
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 299
  ClientWidth = 381
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 16
  object BitBtn1: TBitBtn
    Left = 146
    Top = 261
    Width = 77
    Height = 30
    TabOrder = 0
    Kind = bkOK
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 381
    Height = 249
    Align = alTop
    BevelInner = bvLowered
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    object Viewer: THtmlViewer
      Left = 1
      Top = 1
      Width = 379
      Height = 247
      Enabled = False
      TabOrder = 0
      Align = alClient
      BorderStyle = htSingle
      CharSet = DEFAULT_CHARSET
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
end
