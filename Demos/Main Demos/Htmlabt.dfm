object AboutBox: TAboutBox
  Left = 740
  Top = 213
  BorderIcons = [biSystemMenu]
  Caption = 'About'
  ClientHeight = 209
  ClientWidth = 353
  Color = clWindow
  Constraints.MinHeight = 265
  Constraints.MinWidth = 375
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  DesignSize = (
    353
    209)
  TextHeight = 16
  object BitBtn1: TBitBtn
    Left = 270
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 0
    ExplicitLeft = 291
    ExplicitTop = 193
  end
  object Viewer: THtmlViewer
    Left = 8
    Top = 8
    Width = 337
    Height = 162
    BorderStyle = htNone
    HistoryMaxCount = 0
    HtOptions = []
    NoSelect = True
    PrintMarginBottom = 2.000000000000000000
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintScale = 1.000000000000000000
    ScrollBars = ssNone
    Text = ''
    ViewImages = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Enabled = False
    ParentColor = True
    ParentFont = True
    TabOrder = 1
    ExplicitWidth = 346
    ExplicitHeight = 179
  end
end
