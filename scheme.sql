BEGIN;

CREATE TABLE rubric
    ( id serial PRIMARY KEY
    , name varchar NOT NULL
    );

CREATE TABLE sample
    ( id serial PRIMARY KEY
    , name varchar NOT NULL
    , rubric_id integer REFERENCES rubric (id)
    , preinvestigation varchar[] NOT NULL DEFAULT '{}'
    , investigation varchar[] NOT NULL DEFAULT '{}'
    , trial varchar[] NOT NULL DEFAULT '{}'
    );

-- TEST DATA ------------------------------------------------------------------

INSERT INTO rubric (name) VALUES
('Убийство новорожденных'),
('Убийство с расчленением'),
('Убийство без сокрытия следов преступления'),
('Убийство с инсценировкой иного события');


INSERT INTO "case" (name, rubric_id) VALUES
('есть следы помады', 2),
('волосы всклокочены', 2),
('нет свидетелей', 2),
('подозреваемый задержан', 3),
('несколько подозреваемых', 3),
('делают вид, что ничего не было', 4),
('у всех хитрый прищур', 4);


END;

