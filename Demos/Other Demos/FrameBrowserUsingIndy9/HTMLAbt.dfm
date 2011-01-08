object AboutBox: TAboutBox
  Left = 245
  Top = 137
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 209
  ClientWidth = 306
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
    Left = 114
    Top = 170
    Width = 77
    Height = 30
    TabOrder = 0
    Kind = bkOK
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 306
    Height = 161
    Align = alTop
    BevelInner = bvLowered
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    object Viewer: THtmlViewer
      Left = 1
      Top = 1
      Width = 304
      Height = 159
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
      PrintMarginBottom = 2
      PrintMarginLeft = 2
      PrintMarginRight = 2
      PrintMarginTop = 2
      PrintScale = 1
      ScrollBars = ssNone
      ViewImages = False
    end
  end
end
