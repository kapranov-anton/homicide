BEGIN;

CREATE TABLE rubric
(
  id serial PRIMARY KEY,
  name varchar NOT NULL
);

CREATE TABLE param
(
  id serial PRIMARY KEY,
  name varchar NOT NULL,
  rubric_id integer REFERENCES rubric (id)
);

CREATE TYPE stage AS ENUM ('preinvestigation', 'investigation', 'trial');

CREATE TABLE advice
(
  id serial PRIMARY KEY,
  content text NOT NULL
);

CREATE TABLE advice_param
(
  advice_id integer REFERENCES advice (id),
  param_id  integer REFERENCES param  (id)
);

-- TEST DATA ------------------------------------------------------------------

INSERT INTO rubric (name) VALUES
('Убийство новорожденных'),
('Убийство с расчленением'),
('Убийство без сокрытия следов преступления'),
('Убийство с инсценировкой иного события');


INSERT INTO param (name, rubric_id) VALUES
('есть следы помады', 2),
('волосы всклокочены', 2),
('нет свидетелей', 2),
('подозреваемый задержан', 3),
('несколько подозреваемых', 3),
('делают вид, что ничего не было', 4),
('у всех хитрый прищур', 4);


END;

