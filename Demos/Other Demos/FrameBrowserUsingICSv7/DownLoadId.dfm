object DownLoadForm: TDownLoadForm
  Left = 645
  Top = 221
  BorderIcons = [biSystemMenu]
  Caption = 'DownLoad'
  ClientHeight = 125
  ClientWidth = 307
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 24
    Top = 16
    Width = 33
    Height = 13
    Caption = 'Status:'
  end
  object Label3: TLabel
    Left = 24
    Top = 40
    Width = 47
    Height = 13
    Caption = 'Time Left:'
  end
  object Status: TLabel
    Left = 88
    Top = 16
    Width = 201
    Height = 13
    AutoSize = False
  end
  object TimeLeft: TLabel
    Left = 88
    Top = 40
    Width = 3
    Height = 13
  end
  object CancelButton: TButton
    Left = 112
    Top = 80
    Width = 89
    Height = 25
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = CancelButtonClick
  end
end
