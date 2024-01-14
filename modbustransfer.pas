unit ModbusTransfer;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ZDataset;

type
  TMbtCustomAction = class
    protected
      FName: String;
    public
      procedure Execute; virtual; abstract;
    published
      property Name: String read FName write FName;
  end;

  TMbtCutomOutput = class
    protected
      FName: String;

  end;

  TMbtSqlExecAction = class(TMbtCustomAction)
    protected
      Query: TZQuery;
      procedure SetSQL(NewSQL: String);
      function GetSQL: String;
    published
      property SQL: String read GetSQL write SetSQL;
  end;

  TMbtSqlSelectAction = class(TMbtCustomAction)
    protected
      Query: TZQuery;
      procedure SetSQL(NewSQL: String);
      function GetSQL: String;
    published
      property SQL: String read GetSQL write SetSQL;
  end;

  TMbtModbusAction = class(TMbtCustomAction)
    protected
      FPort:

  end;

implementation

procedure TMbtSqlExecAction.SetSQL(NewSQL: String);
begin
  Query.SQL.Text := NewSQL;
end;

function TMbtSqlExecAction.GetSQL: String;
begin
  Result := Query.SQL.Text;
end;

procedure TMbtSqlSelectAction.SetSQL(NewSQL: String);
begin
  Query.SQL.Text := NewSQL;
end;

function TMbtSqlSelectAction.GetSQL: String;
begin
  Result := Query.SQL.Text;
end;

end.

