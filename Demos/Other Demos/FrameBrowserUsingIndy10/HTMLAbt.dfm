object AboutBox: TAboutBox
  Left = 245
  Top = 137
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 234
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    338
    234)
  PixelsPerInch = 96
  TextHeight = 16
  object BitBtn1: TBitBtn
    Left = 130
    Top = 195
    Width = 77
    Height = 30
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 0
    ExplicitTop = 233
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 338
    Height = 186
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvLowered
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitWidth = 495
    ExplicitHeight = 253
    object Viewer: THtmlViewer
      Left = 1
      Top = 1
      Width = 336
      Height = 184
      Enabled = False
      TabOrder = 0
      Align = alClient
      BorderStyle = htSingle
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
      QuirksMode = qmDetect
      ScrollBars = ssNone
      ViewImages = False
      ExplicitWidth = 493
      ExplicitHeight = 251
    end
  end
end
