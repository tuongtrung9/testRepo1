object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'REST Client with Token Management'
  ClientHeight = 500
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Size = 8
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 684
    Height = 150
    Caption = 'Authentication'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 48
      Height = 13
      Caption = 'Username:'
    end
    object Label2: TLabel
      Left = 16
      Top = 51
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object edtUsername: TEdit
      Left = 80
      Top = 21
      Width = 200
      Height = 21
      TabOrder = 0
      Text = 'user1'
    end
    object edtPassword: TEdit
      Left = 80
      Top = 48
      Width = 200
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
      Text = 'password1'
    end
    object btnLogin: TButton
      Left = 16
      Top = 75
      Width = 75
      Height = 25
      Caption = 'Login'
      TabOrder = 2
      OnClick = btnLoginClick
    end
    object btnRefreshToken: TButton
      Left = 97
      Top = 75
      Width = 100
      Height = 25
      Caption = 'Refresh Token'
      TabOrder = 3
      OnClick = btnRefreshTokenClick
    end
    object btnGetProtectedData: TButton
      Left = 16
      Top = 106
      Width = 120
      Height = 25
      Caption = 'Get Protected Data'
      TabOrder = 4
      OnClick = btnGetProtectedDataClick
    end
    object btnClearTokens: TButton
      Left = 203
      Top = 75
      Width = 100
      Height = 25
      Caption = 'Clear Tokens'
      TabOrder = 5
      OnClick = btnClearTokensClick
    end
    object btnShowTokens: TButton
      Left = 354
      Top = 75
      Width = 100
      Height = 25
      Caption = 'Show Tokens'
      TabOrder = 6
      OnClick = btnShowTokensClick
    end
  end
  object MemoResponse: TMemo
    Left = 8
    Top = 164
    Width = 684
    Height = 328
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object RESTClient1: TRESTClient
    BaseURL = 'http://localhost:3000'
    Params = <>
    Left = 600
    Top = 24
  end
  object RESTRequest1: TRESTRequest
    Client = RESTClient1
    Params = <>
    Response = RESTResponse1
    Left = 600
    Top = 72
  end
  object RESTResponse1: TRESTResponse
    Left = 600
    Top = 120
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 648
    Top = 24
  end
end