object AuthForm: TAuthForm
  Left = 470
  Top = 309
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'AuthForm'
  ClientHeight = 209
  ClientWidth = 205
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 72
    Width = 51
    Height = 13
    Caption = 'User name'
  end
  object Label2: TLabel
    Left = 16
    Top = 112
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object LabelRealm: TLabel
    Left = 19
    Top = 23
    Width = 172
    Height = 43
    AutoSize = False
    Caption = 'Realm'
    WordWrap = True
  end
  object AuthUsername: TEdit
    Left = 16
    Top = 88
    Width = 175
    Height = 21
    TabOrder = 0
  end
  object AuthPassword: TEdit
    Left = 18
    Top = 131
    Width = 173
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object OKBtn: TBitBtn
    Left = 16
    Top = 168
    Width = 81
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
    OnClick = OKBtnClick
  end
  object BitBtn2: TBitBtn
    Left = 110
    Top = 168
    Width = 81
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
end
