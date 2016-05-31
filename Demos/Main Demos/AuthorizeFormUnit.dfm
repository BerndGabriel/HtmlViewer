object AuthorizeForm: TAuthorizeForm
  Left = 470
  Top = 309
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'AuthorizeForm'
  ClientHeight = 209
  ClientWidth = 287
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 65
    Width = 51
    Height = 13
    Caption = 'User name'
  end
  object Label2: TLabel
    Left = 24
    Top = 113
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object LabelRealm: TLabel
    Left = 24
    Top = 16
    Width = 241
    Height = 43
    AutoSize = False
    Caption = 'Realm'
    WordWrap = True
  end
  object AuthUsername: TEdit
    Left = 24
    Top = 81
    Width = 241
    Height = 21
    TabOrder = 0
  end
  object AuthPassword: TEdit
    Left = 24
    Top = 129
    Width = 241
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object OKBtn: TBitBtn
    Left = 24
    Top = 168
    Width = 91
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
    OnClick = OKBtnClick
  end
  object BitBtn2: TBitBtn
    Left = 174
    Top = 168
    Width = 91
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
end
