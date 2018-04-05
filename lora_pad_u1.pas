//Copyright 2018 Andrey S. Ionisyan (anserion@gmail.com)
//
//Licensed under the Apache License, Version 2.0 (the "License");
//you may not use this file except in compliance with the License.
//You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//Unless required by applicable law or agreed to in writing, software
//distributed under the License is distributed on an "AS IS" BASIS,
//WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//See the License for the specific language governing permissions and
//limitations under the License.

//учебный шаблон сервера идентификации с нулевым разглашением (проверяющий)
//по протоколу Фиата-Шамира

//Общая постановка задачи.
//Доказывающая сторона владеет секретом S (число)
//Проверяющая сторона должна убедиться в наличии S у доказывающей стороны в
//условиях отсутствия доступа непосредственно к S.
//Проверяющая сторона не имеет возможности доказать третьей стороне наличие S
//у доказывающей стороны

// протокол идентификации с нулевым разглашением Фиата-Шамира
// 1. Центр распределения ключей генерирует два простых числа p,q
//    которые сообщаются доказывающей стороне (секретно)
//    и их произведение n=p*q (публично).
//    (в данной программе p и q не генерируются, n полагается известным всем)
//    доп. условие, повышающее качество алгоритма: p,q сравнимы с 3 по модулю 4
//    криптостойкость алгоритма основана на сложности операции
//    извлечения корня квадратного в конечных полях
// 2. Доказывающая сторона генерирует и оглашает публичное свойство секрета:
//    V=(S*S) mod n
// 3. Доказывающая сторона генерирует произвольное (случайное) число R,
//    пригодное для дальнейшей работы алгоритма 0<r<n.
// 4. Доказывающая сторона публикует значение X=(R*R) mod n.
// 5. Проверяющая сторона генерирует и публикует случайный бит E.
// 6. Если E=0, то доказывающая сторона публикует значение Y=R,
//    если E=1, то доказывающая сторона публикует значение Y=(R*S) mod n.
// 7. Если E=0, то проверяющая сторона сравнивает X и (Y*Y) mod n на равенство
//    Если E=1, то проверяющая сторона сравнивает (X*V) mod n и (Y*Y) mod n на равенство
// 8. В случае неравенства протокол прекращает работу (срыв доказательства)
// 9. Шаги 3-8 повторяются t раз (достаточно 20-40 раз) для убеждения проверяющего.
//    Вероятность обмана проверяющего при этом равна 2^(-t), т.е. очень мала.

unit Lora_pad_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Btn_OK: TButton;
    BTN_reset: TButton;
    Edit_GSN: TEdit;
    Edit_prove: TEdit;
    Edit_step: TEdit;
    Edit_answer: TEdit;
    Edit_key_id: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label_pad_state: TLabel;
    Label_CMD: TLabel;
    Label7: TLabel;
    Memo1: TMemo;
    procedure Btn_OKClick(Sender: TObject);
    procedure BTN_resetClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  state,step_num,good_steps_num:integer;
  N,V,X,Y,E:integer;
  prove:real;
  Y_ok:boolean;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Btn_OKClick(Sender: TObject);
begin
  randomize;
  N:=StrToInt(copy(Edit_key_id.text,1,4));
  V:=StrToInt(copy(Edit_key_id.text,5,4));
  case state of
  0: begin
       Y:=StrToInt(Edit_answer.text);
       if E=0 then Y_ok:=(((Y*Y)mod N)=X);
       if E=1 then Y_ok:=(((Y*Y)mod N)=((X*V)mod N));
       if Y=0 then Y_ok:=false;
       if Y_ok then
       begin
         step_num:=step_num+1;
         prove:=1-(1-prove)/2;
       end else
       begin
         step_num:=0;
         prove:=0;
       end;
       good_steps_num:=StrToInt(Edit_GSN.text);
       if step_num>=good_steps_num then Label_pad_state.caption:='Открыто'
                                   else Label_pad_state.caption:='Закрыто';
       Label_CMD.Caption:='Зеленый код';
       Edit_answer.text:='0';
       state:=1;
     end;
  1: begin
       X:=StrToInt(Edit_answer.text);
       E:=random(2);
       if E=0 then Label_CMD.caption:='Желтый код';
       if E=1 then Label_CMD.caption:='Красный код';
       Edit_answer.text:='0';
       state:=0;
     end;
  end;
  Edit_step.text:=IntToStr(step_num);
  Edit_prove.text:=FloatToStr(prove);
end;

procedure TForm1.BTN_resetClick(Sender: TObject);
begin
  step_num:=0; Edit_step.text:=IntToStr(step_num);
  prove:=0; Edit_prove.text:=FloatToStr(prove);
  Label_CMD.Caption:='Зеленый код';
  Edit_answer.text:='0';
  state:=1;
end;

end.

