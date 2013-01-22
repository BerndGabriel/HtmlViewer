object AuthForm: TAuthForm
  Left = 470
  Top = 309
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'AuthForm'
  ClientHeight = 155
  ClientWidth = 209
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
    Left = 24
    Top = 16
    Width = 51
    Height = 13
    Caption = 'User name'
  end
  object Label2: TLabel
    Left = 24
    Top = 56
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object AuthUsername: TEdit
    Left = 24
    Top = 32
    Width = 161
    Height = 21
    TabOrder = 0
  end
  object AuthPassword: TEdit
    Left = 24
    Top = 72
    Width = 161
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object OKBtn: TBitBtn
    Left = 24
    Top = 112
    Width = 75
    Height = 25
    TabOrder = 2
    OnClick = OKBtnClick
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 112
    Top = 112
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
end
