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
values (100, 'Леса, расположенные на особо охраняемых природных территориях', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (120, 'Леса, расположенные в водоохранных зонах', 1);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (130, 'Леса, выполняющие функции защиты природных и иных объектов', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (131, 'Леса, расположенные в первом и втором поясах зон санитарной охраны источников питьевого и хозяйственно-бытового водоснабжения', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (132, 'Защитные полосы лесов вдоль железнодорожных путей общего пользования, федеральных автомобильных дорог общего пользования и автомобильных дорог общего пользования, находящихся в собственности субъектов Российской Федерации', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (133, 'Зеленые зоны', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (134, 'Лесопарковые зоны', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (135, 'Городские леса', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (139, 'Леса, расположенные в первой, второй и третьей зон округов санитарной (горно-санитарной) охраны лечебно-оздоровительных местностей и курортов', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (140, 'Ценные леса', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (141, 'Государственные защитные лесные полосы', 2);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (142, 'Противоэрозионные леса', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (143, 'Леса, расположенные в пустынных, полупустынных, лесостепных, лесо-тундровых зонах, степях, горах', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (144, 'Леса, имеющие научное или исторические значение', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (145, 'Орехово-промысловые зоны', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (146, 'Лесные плодовые насаждения', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (147, 'Ленточные боры', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (148, 'Запретные полосы лесов, расположенные вдоль водных объектов', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (149, 'Нерестоохранные полосы лесов', 3);
Insert into protect_category (protect_category_code, protect_category_rus, protect_category_code_dop) 
values (150, 'Особо защитные участки лесов', 3);
