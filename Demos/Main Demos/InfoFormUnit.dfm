object InfoForm: TInfoForm
  Left = 298
  Top = 180
  Caption = 'Info'
  ClientHeight = 336
  ClientWidth = 468
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnResize = FormResize
  DesignSize = (
    468
    336)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TBitBtn
    Left = 199
    Top = 297
    Width = 65
    Height = 25
    Anchors = [akLeft, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 0
  end
  object mmoInfo: TMemo
    Left = 16
    Top = 16
    Width = 438
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'mmoInfo')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
end
