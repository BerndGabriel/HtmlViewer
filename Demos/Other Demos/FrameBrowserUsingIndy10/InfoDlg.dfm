object InfoForm: TInfoForm
  Left = 298
  Top = 180
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Proxy'
  ClientHeight = 323
  ClientWidth = 463
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    463
    323)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 26
    Height = 13
    Caption = 'Proxy'
  end
  object OKBurron: TBitBtn
    Left = 199
    Top = 284
    Width = 65
    Height = 25
    Anchors = [akLeft, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 0
  end
  object mmoInfo: TMemo
    Left = 24
    Top = 32
    Width = 416
    Height = 233
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'mmoInfo')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    ExplicitWidth = 241
    ExplicitHeight = 117
  end
end
