object DownLoadForm: TDownLoadForm
  Left = 645
  Top = 221
  BorderIcons = [biSystemMenu]
  Caption = 'DownLoad'
  ClientHeight = 107
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Label2: TLabel
    Left = 30
    Top = 20
    Width = 40
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Status:'
  end
  object Label3: TLabel
    Left = 30
    Top = 49
    Width = 58
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Time Left:'
  end
  object Status: TLabel
    Left = 108
    Top = 20
    Width = 248
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    AutoSize = False
  end
  object TimeLeft: TLabel
    Left = 108
    Top = 49
    Width = 3
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
  end
  object CancelButton: TButton
    Left = 138
    Top = 98
    Width = 109
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = CancelButtonClick
  end
end
