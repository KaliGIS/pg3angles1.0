--
-- Subor: pg3angles--0.1.sql  Autor: Bc. Martin Kalivoda  Datum: 10.3.2014
--
-- Tento subor predstavuje skript na rozsirenie funkcionality databazoveho systemu
-- PostgreSQL a jeho priestorovej nadstavby PostGIS na pracu s nepravidelnymi trojuholnikovymi
-- sietami (TIN) ako extenzie (extension) pg3angles. 
--

-- Pomocou prikazu \echo zabranime nacitaniu prikazov tohto scriptu samostatne (mimo extenzie)
\echo Use "CREATE EXTENSION pg3angles" to load this file. \quit

CREATE OR REPLACE FUNCTION tin_createfrompoints (tintablename text, pntstable text, pntsgeomcol text) RETURNS void AS
$$
BEGIN
  EXECUTE 'CREATE TABLE ' || tintablename || ' (tid SERIAL PRIMARY KEY, geom geometry)';
  EXECUTE 'SELECT TIN_GrantTinBehaviour(''' || tintablename || ''')';
  EXECUTE 'INSERT INTO ' || tintablename || ' (geom)
    SELECT (elementy).geom FROM (SELECT ST_Dump(ST_DelaunayTriangles(ST_Collect(' || pntsgeomcol || '), 0, 2)) AS elementy 
		FROM ' || pntstable || ') AS subselekcia';
END
$$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION tin_prepareforpg3angles() RETURNS trigger AS
$$
DECLARE
	bod1 geometry;
	bod2 geometry;
	bod3 geometry;
	b1 text;
	b2 text;
	b3 text;
	testbod geometry;
	je1 integer;
	je2 integer;
	je3 integer;
	i integer;
	tid integer;
	pole integer[];
	r RECORD;
	tabulka text;
  tin text;
BEGIN
	tabulka := TG_ARGV[0];
  tin := TG_ARGV[1];
	bod1 := ST_PointN(ST_Boundary(NEW.geom), 1);
	bod2 := ST_PointN(ST_Boundary(NEW.geom), 2);
	bod3 := ST_PointN(ST_Boundary(NEW.geom), 3);
	b1 := ' '' ' || ST_AsText(bod1) || ' '' ';
	b2 := ' '' ' || ST_AsText(bod2) || ' '' ';
	b3 := ' '' ' || ST_AsText(bod3) || ' '' ';
	je1 := 0;
	je2 := 0;
	je3 := 0;
	tid := NEW.tid;
	IF tid > 1 THEN
		FOR r IN EXECUTE 'SELECT * FROM ' || tabulka
		LOOP
			testbod := r.point;
			pole := r.triangles;
			IF ST_Equals(testbod, bod1) THEN
				EXECUTE 'UPDATE ' || tabulka || ' SET triangles = '' ' || (array_append(pole, tid))::text || ' '' WHERE pid = ' || r.pid;
				je1 := 1;
			ELSIF ST_Equals(testbod, bod2) THEN
				EXECUTE 'UPDATE ' || tabulka || ' SET triangles = '' ' || (array_append(pole, tid))::text || ' '' WHERE pid = ' || r.pid;
				je2 := 1;
			ELSIF ST_Equals(testbod, bod3) THEN
				EXECUTE 'UPDATE ' || tabulka || ' SET triangles = '' ' || (array_append(pole, tid))::text || ' '' WHERE pid = ' || r.pid;
				je3 := 1;
			END IF;
		END LOOP;
	END IF;
	IF je1 = 0 THEN
		EXECUTE 'INSERT INTO ' || tabulka || ' (point, triangles) VALUES
		(' || b1 || ', ' || ' '' ' || (array_append(ARRAY[]::int[], tid))::text || ' '')';
	END IF;
	IF je2 = 0 THEN
		EXECUTE 'INSERT INTO ' || tabulka || ' (point, triangles) VALUES
		(' || b2 || ', ' || ' '' ' || (array_append(ARRAY[]::int[], tid))::text || ' '')';
	END IF;
	IF je3 = 0 THEN
		EXECUTE 'INSERT INTO ' || tabulka || ' (point, triangles) VALUES
		(' || b3 || ', ' || ' '' ' || (array_append(ARRAY[]::int[], tid))::text || ' '')';
	END IF;
  EXECUTE 'UPDATE ' || tin || ' SET geom = TIN_ExtendM(geom, tid) WHERE tid = ' || NEW.tid;
	RETURN NEW;
END
$$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION tin_granttinbehaviour (tintable text) RETURNS void AS
$$
DECLARE
	nazov text;
BEGIN
	nazov := concat(tintable, '_points');
	EXECUTE format('CREATE TABLE %s (pid SERIAL PRIMARY KEY, point geometry, triangles integer[])', nazov);
  EXECUTE 'CREATE TRIGGER TIN_PrepareForpg3angles_t_' || tintable ||
    ' AFTER INSERT ON ' || tintable || ' FOR EACH ROW EXECUTE PROCEDURE TIN_PrepareForpg3angles(''' || nazov || ''', ''' || tintable || ''')';
END
$$
LANGUAGE plpgsql;
  
CREATE OR REPLACE FUNCTION tin_editneighboursinfo(tintable text) RETURNS void AS
$$
DECLARE
  M float8;
  tid integer;
BEGIN
  EXECUTE 'SELECT ST_M(ST_PointN(ST_Boundary(geom), 1)) FROM ' || tintable || ' LIMIT 1' INTO M; 
  IF M <> NULL THEN
    EXECUTE ' SELECT TIN_GetTid(geom) FROM ' || tintable || ' LIMIT 1' INTO tid;
    IF tid <> 0 THEN
      EXECUTE 'WITH geomgrupCTE AS(SELECT TIN_EditNeighboursInfo(array_agg(susedne.geom), kmenovy.geom) AS edited, kmenovy.geom AS kmentroj
        FROM ' || tintable || ' AS kmenovy, ' || tintable || ' AS susedne
        WHERE kmenovy.geom && susedne.geom
        GROUP BY kmenovy.tid)
        UPDATE ' || tintable || ' SET geom = (SELECT edited FROM geomgrupCTE
          WHERE TIN_GetTid(' || tintable || '.geom) = TIN_GetTid(geomgrupCTE.edited))';
    ELSE
      RAISE EXCEPTION 'tid of the first triangle is zero' USING HINT = 'Use TIN_ExtendM which match the triangles with their tids';
    END IF;
  ELSE
    RAISE EXCEPTION 'M value is null' USING HINT = 'Use TIN_ExtendM to extend dimension of triangles to 4D';
  END IF;
END
$$
LANGUAGE plpgsql;
  
CREATE OR REPLACE FUNCTION tin_area3d(triangle geometry)
  RETURNS double precision AS
'$libdir/TIN_functions', 'TIN_Area3D'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_centroid(triangle geometry)
  RETURNS geometry AS
'$libdir/TIN_functions', 'TIN_Centroid'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_partialderivativesoftriangle(triangle geometry)
  RETURNS double precision[] AS
'$libdir/TIN_functions', 'TIN_PartialDerivativesOfTriangle'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_slopeoftriangle(triangle geometry)
  RETURNS double precision AS
'$libdir/TIN_functions', 'TIN_SlopeOfTriangle'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_aspectoftriangle(triangle geometry)
  RETURNS double precision AS
'$libdir/TIN_functions', 'TIN_AspectOfTriangle'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_linearz(triangle geometry, point geometry)
  RETURNS double precision AS
'$libdir/TIN_functions', 'TIN_LinearZ'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_extendm(triangle geometry, tidentifier double precision)
  RETURNS geometry AS
'$libdir/TIN_functions', 'TIN_ExtendM'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_gettid(triangle geometry)
  RETURNS integer AS
'$libdir/TIN_functions', 'TIN_GetTid'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_editneighboursinfo(trianglesarray geometry[], triangle geometry)
  RETURNS geometry AS
'$libdir/TIN_functions', 'TIN_EditNeighboursInfo'
  LANGUAGE c STRICT; 
  
CREATE OR REPLACE FUNCTION tin_getneighbourstids(triangle geometry)
  RETURNS integer[] AS
'$libdir/TIN_functions', 'TIN_GetNeighboursTids'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_flip(triangles geometry[])
  RETURNS geometry[] AS
'$libdir/TIN_functions', 'TIN_Flip'
  LANGUAGE c STRICT; 
  
CREATE OR REPLACE FUNCTION tin_partialderivativesonvertex(point geometry, neighbours geometry[])
  RETURNS double precision[] AS
'$libdir/TIN_functions', 'TIN_PartialDerivativesOnVertex'
  LANGUAGE c STRICT;  

CREATE OR REPLACE FUNCTION tin_slopeonvertex(point geometry, neighbours geometry[])
  RETURNS double precision AS
'$libdir/TIN_functions', 'TIN_SlopeOnVertex'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_aspectonvertex(point geometry, neighbours geometry[])
  RETURNS double precision AS
'$libdir/TIN_functions', 'TIN_AspectOnVertex'
  LANGUAGE c STRICT;

CREATE OR REPLACE FUNCTION tin_quasiquadraticz(triangle geometry, neighbours1 geometry[], neighbours2 geometry[], neighbours3 geometry[], point geometry)
  RETURNS double precision AS
'$libdir/TIN_functions', 'TIN_QuasiQuadraticZ'
  LANGUAGE c STRICT;

CREATE OR REPLACE FUNCTION tin_quasiquadraticderivatives(triangle geometry, neighbours1 geometry[], neighbours2 geometry[], neighbours3 geometry[], point geometry)
  RETURNS double precision[] AS
'$libdir/TIN_functions', 'TIN_QuasiQuadraticDerivatives'
  LANGUAGE c STRICT;

CREATE OR REPLACE FUNCTION tin_normalsangle(normal1 double precision[], normal2 double precision[])
  RETURNS double precision AS
'$libdir/TIN_functions', 'TIN_NormalsAngle'
  LANGUAGE c  STRICT;
  
CREATE OR REPLACE FUNCTION tin_isconvexhull(triangle1 geometry, triangle2 geometry)
  RETURNS boolean AS
'$libdir/TIN_functions', 'TIN_IsConvexHull'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_contours(triangle geometry, deltaz double precision)
  RETURNS geometry[] AS
'$libdir/TIN_functions', 'TIN_Contours'
  LANGUAGE c STRICT;