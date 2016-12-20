CREATE TABLE protect_category
(
  protect_category_code integer NOT NULL,
  protect_category_rus character varying,
  protect_category_code_dop integer NOT NULL,
  CONSTRAINT protect_category_pkey PRIMARY KEY (protect_category_code)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE protect_category
  OWNER TO postgres;
  
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (100, '����, ������������� �� ����� ���������� ��������� �����������', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (120, '����, ������������� � ������������ �����', 1);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (130, '����, ����������� ������� ������ ��������� � ���� ��������', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (131, '����, ������������� � ������ � ������ ������ ��� ���������� ������ ���������� ��������� � ������������-�������� �������������', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (132, '�������� ������ ����� ����� ��������������� ����� ������ �����������, ����������� ������������� ����� ������ ����������� � ������������� ����� ������ �����������, ����������� � ������������� ��������� ���������� ���������', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (133, '������� ����', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (134, '������������ ����', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (135, '��������� ����', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (139, '����, ������������� � ������, ������ � ������� ��� ������� ���������� (�����-����������) ������ �������-��������������� ���������� � ��������', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (140, '������ ����', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (141, '��������������� �������� ������ ������', 2);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (142, '����������������� ����', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (143, '����, ������������� � ���������, �������������, �����������, ����-��������� �����, ������, �����', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (144, '����, ������� ������� ��� ������������ ��������', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (145, '�������-����������� ����', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (146, '������ �������� ����������', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (147, '��������� ����', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (148, '��������� ������ �����, ������������� ����� ������ ��������', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (149, '��������������� ������ �����', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (150, '����� �������� ������� �����', 3);
