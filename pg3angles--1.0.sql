--
-- Subor: pg3angles--1.0.sql  Autor: Bc. Martin Kalivoda  Datum: 15.5.2014
--
-- Tento subor implementuje funkcie extenzie (extension) pg3angles do databazoveho
-- systemu PostgreSQL a jeho priestorovej nadstavby PostGIS. Dotknute funkcie
-- rozsiruju moznosti prace s nepravidelnymi trojuholnikovymi sietami (TIN).
--

-- Pomocou prikazu \echo zabranime nacitaniu prikazov tohto scriptu samostatne (mimo extenzie)
\echo Use "CREATE EXTENSION pg3angles" to load this file. \quit

CREATE OR REPLACE FUNCTION tin_createfrompointsdt (tintablename text, pntstable text, pntsgeomcol text) RETURNS void AS
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
	tintable_points text;
BEGIN
	tintable_points := concat(tintable, '_points');
	EXECUTE format('CREATE TABLE %s (pid SERIAL PRIMARY KEY, point geometry, triangles integer[])', tintable_points);
  EXECUTE 'CREATE TRIGGER TIN_PrepareForpg3angles_t_' || tintable ||
    ' AFTER INSERT ON ' || tintable || ' FOR EACH ROW EXECUTE PROCEDURE TIN_PrepareForpg3angles(''' || tintable_points || ''', ''' || tintable || ''')';
END
$$
LANGUAGE plpgsql;
  
CREATE OR REPLACE FUNCTION tin_editneighboursinfo(tintable text) RETURNS void AS
$$
DECLARE
  M float8;
  tid integer;
  r record;
  triangles geometry[];
BEGIN
  EXECUTE 'SELECT ST_M(ST_PointN(ST_Boundary(geom), 1)) FROM ' || tintable || ' LIMIT 1' INTO M; 
  IF M IS NOT NULL THEN
    EXECUTE ' SELECT TIN_GetTid(geom) FROM ' || tintable || ' LIMIT 1' INTO tid;
    IF tid <> 0 THEN
      FOR r IN EXECUTE 'SELECT geom, ST_MakePolygon(ST_Boundary(geom)) AS g FROM ' || tintable
      LOOP
        EXECUTE 'SELECT array_agg(geom) FROM ' || tintable || ' WHERE ST_Relate(ST_MakePolygon(ST_Boundary(geom)), 
        ST_GeomFromText(''' || ST_AsText(r.g) || '''), ''FF*F1****'')' INTO triangles;  
        EXECUTE 'UPDATE ' || tintable || ' SET geom = ST_GeomFromText(''' || ST_AsText(TIN_EditNeighboursInfo(triangles, r.geom)) ||
        ''') WHERE ST_Equals(ST_MakePolygon(ST_Boundary(geom)), ST_GeomFromText(''' || ST_AsText(r.g) || '''))';
      END LOOP;
    ELSE
      RAISE EXCEPTION 'tid of the first triangle is zero' USING HINT = 'Use TIN_ExtendM which match the triangles with their tids';
    END IF;
  ELSE
    RAISE EXCEPTION 'M value is null' USING HINT = 'Use TIN_ExtendM to extend dimension of triangles to 4D';
  END IF;
END
$$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION tin_pntcommontriangles(triangle geometry, tintable text, vertexnum integer) RETURNS geometry[] AS
$$
DECLARE
	bod geometry;
	pnttable text;
	tidecka integer[];
	r RECORD;
	vystup geometry[];
BEGIN
	SELECT ST_PointN(ST_Boundary(triangle), vertexnum) INTO bod;
	pnttable := tintable || '_points';
	FOR r IN EXECUTE 'SELECT point, triangles FROM ' || pnttable
	LOOP
		IF ST_Equals(bod, r.point) THEN
				tidecka := r.triangles;
		END IF;
	END LOOP;
	EXECUTE 'SELECT array_agg(geom) FROM ' || tintable || ' WHERE tid IN (' || array_to_string(tidecka, ',') || ')' INTO vystup;
	RETURN vystup;
END
$$
LANGUAGE plpgsql;
  
CREATE OR REPLACE FUNCTION TIN_LocalOptimizingIter(tintable text, iternum integer) RETURNS void AS
$$
DECLARE
	newtab text;
	pom integer;
	r record;
	riadok record;
	linnormal float8[];
	kknormal float8[];
	linnormal2 float[];
	kknormal2 float8[];
	susedia integer[];
	uhol float8; 
	uhol2 float8;
	i integer;
	j integer;
	trojuholniky geometry[];
	fliped geometry[];
	body geometry[];
	susedia1 geometry[];
	susedia2 geometry[];
	susedia3 geometry[];
	sustroj geometry;
	centroid geometry;
	tid integer;
	stid integer;
	sumn float8;
	sumo float8;
	dsum float8;
	u1 float8;
	u2 float8;
	b boolean;
	stav integer;
	kurzor refcursor;
	pole_tid integer[];
BEGIN
	newtab := tintable || '_details_before_iteration_' || iternum::text;
	EXECUTE 'SELECT EXISTS (SELECT * FROM pg_tables WHERE tablename = ''' || newtab || ''')' INTO b;
	IF b THEN
		EXECUTE 'DROP TABLE ' || newtab;
	END IF;
	EXECUTE 'CREATE TABLE ' || newtab || ' (tid integer, ntid1 integer, ntid2 integer, ntid3 integer, lin_normal float8[], qq_normal float8[], angle float8)';
	FOR r IN EXECUTE 'SELECT * FROM ' || tintable
	LOOP
		linnormal := TIN_PartialDerivativesOfTriangle(r.geom);
		kknormal := TIN_QuasiQuadraticDerivatives(r.geom, 
			TIN_PntCommonTriangles(r.geom, tintable, 1),
			TIN_PntCommonTriangles(r.geom, tintable, 2),
			TIN_PntCommonTriangles(r.geom, tintable, 3),
			TIN_Centroid(r.geom));
		uhol := TIN_NormalsAngle(linnormal, kknormal);
		linnormal := array_append(linnormal, 1::double precision);
		kknormal := array_append(kknormal, 1::double precision);
		linnormal[1] := -1 * linnormal[1];
		linnormal[2] := -1 * linnormal[2];
		kknormal[1] := -1 * kknormal[1];
		kknormal[2] := -1 * kknormal[2];
		susedia := TIN_GetNeighboursTids(r.geom);
		EXECUTE 'INSERT INTO ' || newtab || ' VALUES ( ' || TIN_GetTid(r.geom)::text || ', '|| susedia[1]::text || ', ' || susedia[2]::text || ', 
		' || susedia[3]::text || ', ARRAY[' || array_to_string(linnormal, ',') || '], ARRAY[' || array_to_string(kknormal, ',') || '], ' || uhol::text || ')';
	END LOOP;
	newtab := tintable || '_optimizing_info_' || iternum::text;
	EXECUTE 'SELECT EXISTS (SELECT * FROM pg_tables WHERE tablename = ''' || newtab || ''')' INTO b;
	IF b THEN
		EXECUTE 'DROP TABLE ' || newtab;
	END IF;
	EXECUTE 'CREATE TABLE ' || newtab || ' (tid integer, ntid integer, lin_normal1 float8[], qq_normal1 float8[], angle1 float8, lin_normal2 float8[], qq_normal2 float8[], angle2 float8, sum_new float8, sum_orig float8, delta_sum float8, status integer)';
	FOR r IN EXECUTE 'SELECT * FROM ' || tintable
	LOOP
		susedia := TIN_GetNeighboursTids(r.geom);
		FOR i IN 1..3
		LOOP
			IF susedia[i] <> -1 THEN
				EXECUTE 'SELECT geom FROM ' || tintable || ' WHERE tid = ' || susedia[i]::text INTO sustroj;
				IF TIN_IsConvexHull(r.geom, sustroj) THEN
					tid := TIN_GetTid(r.geom);
					stid := TIN_GetTid(sustroj);
					EXECUTE 'SELECT count(*) FROM ' || newtab || ' WHERE tid = ' || stid::text || ' AND ntid = ' || tid::text INTO pom;
					IF pom = 0 THEN
						trojuholniky := TIN_Flip(ARRAY[r.geom, sustroj]);
            centroid := TIN_Centroid(trojuholniky[1]);
						IF ST_Intersects(centroid, ST_MakePolygon(ST_Boundary(r.geom))) THEN
							kknormal := TIN_QuasiQuadraticDerivatives(r.geom, 
								TIN_PntCommonTriangles(r.geom, tintable, 1),
								TIN_PntCommonTriangles(r.geom, tintable, 2),
								TIN_PntCommonTriangles(r.geom, tintable, 3),
								centroid);
						ELSIF ST_Intersects(centroid, ST_MakePolygon(ST_Boundary(sustroj))) THEN
							kknormal := TIN_QuasiQuadraticDerivatives(sustroj, 
								TIN_PntCommonTriangles(sustroj, tintable, 1),
								TIN_PntCommonTriangles(sustroj, tintable, 2),
								TIN_PntCommonTriangles(sustroj, tintable, 3),
								centroid);
						END IF;
						linnormal := TIN_PartialDerivativesOfTriangle(trojuholniky[1]);
						uhol := TIN_NormalsAngle(linnormal, kknormal);
						linnormal := array_append(linnormal, 1::double precision);
						kknormal := array_append(kknormal, 1::double precision);
						linnormal[1] := -1 * linnormal[1];
						linnormal[2] := -1 * linnormal[2];
						kknormal[1] := -1 * kknormal[1];
						kknormal[2] := -1 * kknormal[2];
            centroid := TIN_Centroid(trojuholniky[2]);
						IF ST_Intersects(centroid, ST_MakePolygon(ST_Boundary(r.geom))) THEN
							kknormal2 := TIN_QuasiQuadraticDerivatives(r.geom, 
								TIN_PntCommonTriangles(r.geom, tintable, 1),
								TIN_PntCommonTriangles(r.geom, tintable, 2),
								TIN_PntCommonTriangles(r.geom, tintable, 3),
								centroid);
						ELSIF ST_Intersects(centroid, ST_MakePolygon(ST_Boundary(sustroj))) THEN
							kknormal2 := TIN_QuasiQuadraticDerivatives(sustroj, 
								TIN_PntCommonTriangles(sustroj, tintable, 1),
								TIN_PntCommonTriangles(sustroj, tintable, 2),
								TIN_PntCommonTriangles(sustroj, tintable, 3),
								centroid);
						END IF;
						linnormal2 := TIN_PartialDerivativesOfTriangle(trojuholniky[2]);
						uhol2 := TIN_NormalsAngle(linnormal2, kknormal2);
						linnormal2 := array_append(linnormal2, 1::double precision);
						kknormal2 := array_append(kknormal2, 1::double precision);
						linnormal2[1] := -1 * linnormal2[1];
						linnormal2[2] := -1 * linnormal2[2];
						kknormal2[1] := -1 * kknormal2[1];
						kknormal2[2] := -1 * kknormal2[2];
						sumn := uhol + uhol2;
						EXECUTE 'SELECT angle FROM ' || tintable || '_details_before_iteration_' || iternum::text || ' WHERE tid = ' || tid INTO u1;
						EXECUTE 'SELECT angle FROM ' || tintable || '_details_before_iteration_' || iternum::text || ' WHERE tid = ' || stid INTO u2;
						sumo := u1 + u2;
						dsum := sumo - sumn;
						IF dsum > 0 THEN
							stav := 1;
						ELSE
							stav := 0;
						END IF;
						EXECUTE 'INSERT INTO ' || newtab || ' VALUES ( ' || tid::text || ', '|| stid::text || ', ARRAY[' || array_to_string(linnormal, ',') || '], 
						ARRAY[' || array_to_string(kknormal, ',')  || '], ' || uhol::text || ', ARRAY[' || array_to_string(linnormal2, ',') || '], 
						ARRAY[' || array_to_string(kknormal2, ',')  || '], ' || uhol2::text || ', ' || sumn::text || ', ' || sumo::text || ', ' || dsum::text || ', ' || stav::text || ')';
					END IF;
				END IF;
			END IF;
		END LOOP;
	END LOOP;
	FOR riadok IN EXECUTE 'SELECT * FROM ' || newtab || ' WHERE delta_sum > 0'
	LOOP
		OPEN kurzor FOR EXECUTE 'SELECT * FROM ' || newtab || ' WHERE delta_sum > 0' FOR UPDATE;
		LOOP
			FETCH NEXT FROM kurzor INTO r;
			EXIT WHEN r IS NULL;
			IF riadok.tid <> r.tid OR riadok.ntid <> r.ntid THEN
				IF r.tid = riadok.tid OR r.tid = riadok.ntid THEN
					IF r.delta_sum < riadok.delta_sum THEN
						EXECUTE 'UPDATE ' || newtab || ' SET status = ' || 0::text || ' WHERE ctid = $1' USING r.ctid;
					END IF;
				ELSIF r.ntid = riadok.tid OR r.ntid = riadok.ntid THEN
					IF r.delta_sum < riadok.delta_sum THEN
						EXECUTE 'UPDATE ' || newtab || ' SET status = ' || 0::text || ' WHERE ctid = $1' USING r.ctid;
					END IF;
				END IF;
			END IF;
		END LOOP;
		CLOSE kurzor;
	END LOOP;
	j := 1;
	FOR riadok IN EXECUTE 'SELECT * FROM ' || newtab || ' WHERE status = 1'
	LOOP
		trojuholniky := ARRAY[]::geometry[];
		susedia := ARRAY[]::integer[];
		body := ARRAY[]::geometry[];
		EXECUTE 'SELECT geom FROM ' || tintable || ' WHERE tid = ' || riadok.tid INTO sustroj;
		trojuholniky := array_append(trojuholniky, sustroj);
		EXECUTE 'SELECT geom FROM ' || tintable || ' WHERE tid = ' || riadok.ntid INTO sustroj;
		trojuholniky := array_append(trojuholniky, sustroj);
		susedia := TIN_GetNeighboursTids(trojuholniky[1]);
		susedia := array_cat(susedia, TIN_GetNeighboursTids(trojuholniky[2]));
		susedia := array_remove(susedia, riadok.tid);
		susedia := array_remove(susedia, riadok.ntid);
		FOR i IN 1..array_length(susedia, 1)
		LOOP
			IF susedia[i] <> -1 AND susedia[i] <> 0 THEN
				EXECUTE 'SELECT geom FROM ' || tintable || ' WHERE tid = ' || susedia[i] INTO sustroj;
				trojuholniky := array_append(trojuholniky, sustroj);
			END IF;
		END LOOP;
		RAISE NOTICE '% % % % % % %', TIN_GetTid(trojuholniky[1]), TIN_GetTid(trojuholniky[2]), TIN_GetTid(trojuholniky[3]), TIN_GetTid(trojuholniky[4]), TIN_GetTid(trojuholniky[5]), TIN_GetTid(trojuholniky[6]), array_length(trojuholniky, 1);
		j := j + 1;
		fliped := TIN_Flip(trojuholniky);
		FOR i IN 1..array_length(fliped, 1)
		LOOP
			EXECUTE 'UPDATE ' || tintable || ' SET geom = '' ' || ST_AsText(fliped[i]) || ' '' WHERE tid = ' || TIN_GetTid(fliped[i]);
		END LOOP;
		body := array_append(body, ST_PointN(ST_Boundary(fliped[1]), 1));
		body := array_append(body, ST_PointN(ST_Boundary(fliped[1]), 2));
		body := array_append(body, ST_PointN(ST_Boundary(fliped[1]), 3));
		body := array_append(body, ST_PointN(ST_Boundary(fliped[2]), 1));
		FOR i IN 1..4
		LOOP
			EXECUTE 'SELECT triangles FROM ' || tintable || '_points WHERE ST_Equals(point, ST_GeomFromText('' ' || ST_AsText(body[i]) || ' ''))' INTO pole_tid;
			pole_tid := array_remove(pole_tid, TIN_GetTid(fliped[1]));
			pole_tid := array_remove(pole_tid, TIN_GetTid(fliped[2]));
			IF ST_Intersects(body[i], ST_Boundary(fliped[1])) THEN
				pole_tid := array_append(pole_tid, TIN_GetTid(fliped[1]));
			END IF;
			IF ST_Intersects(body[i], ST_Boundary(fliped[2])) THEN
				pole_tid := array_append(pole_tid, TIN_GetTid(fliped[2]));
			END IF;
			EXECUTE 'UPDATE ' || tintable || '_points SET triangles = '' ' || pole_tid::text || ' '' WHERE ST_Equals(point, ST_GeomFromText('' ' || ST_AsText(body[i]) || ' ''))';
		END LOOP;
	END LOOP;
	newtab := tintable || '_iterations_info';
	EXECUTE 'SELECT EXISTS (SELECT * FROM pg_tables WHERE tablename = ''' || newtab || ''')' INTO b;
	IF NOT b THEN
		EXECUTE 'CREATE TABLE ' || newtab || ' (iternum integer, mean_angle_before float8, stddev_angle_before float8, min_angle_before float8, 
		max_angle_before float8, mean_angle_after float8, stddev_angle_after float8, min_angle_after float8, max_angle_after float8, flip_count integer)';
	END IF;	
	CREATE TEMPORARY TABLE docasna (uhol float8);
	FOR r IN EXECUTE 'SELECT geom FROM ' || tintable
	LOOP
		linnormal := TIN_PartialDerivativesOfTriangle(r.geom);
		kknormal := TIN_QuasiQuadraticDerivatives(r.geom, 
			TIN_PntCommonTriangles(r.geom, tintable, 1),
			TIN_PntCommonTriangles(r.geom, tintable, 2),
			TIN_PntCommonTriangles(r.geom, tintable, 3),
			TIN_Centroid(r.geom));
		uhol := TIN_NormalsAngle(linnormal, kknormal);
		EXECUTE 'INSERT INTO docasna VALUES (' || uhol || ')';
	END LOOP;
	EXECUTE 'INSERT INTO ' || newtab || ' (iternum) VALUES (' || iternum::text || ')';
	EXECUTE 'SELECT avg(angle) FROM ' || tintable || '_details_before_iteration_' || iternum INTO uhol;
	EXECUTE 'UPDATE ' || newtab || ' SET mean_angle_before = ' || uhol || ' WHERE iternum = ' || iternum;
	EXECUTE 'SELECT stddev(angle) FROM ' || tintable || '_details_before_iteration_' || iternum INTO uhol;
	EXECUTE 'UPDATE ' || newtab || ' SET stddev_angle_before = ' || uhol || ' WHERE iternum = ' || iternum;
	EXECUTE 'SELECT min(angle) FROM ' || tintable || '_details_before_iteration_' || iternum INTO uhol;
	EXECUTE 'UPDATE ' || newtab || ' SET min_angle_before = ' || uhol || ' WHERE iternum = ' || iternum;
	EXECUTE 'SELECT max(angle) FROM ' || tintable || '_details_before_iteration_' || iternum INTO uhol;
	EXECUTE 'UPDATE ' || newtab || ' SET max_angle_before = ' || uhol || ' WHERE iternum = ' || iternum;
	EXECUTE 'SELECT avg(uhol) FROM docasna' INTO uhol;
	EXECUTE 'UPDATE ' || newtab || ' SET mean_angle_after = ' || uhol || ' WHERE iternum = ' || iternum;
	EXECUTE 'SELECT stddev(uhol) FROM docasna' INTO uhol;
	EXECUTE 'UPDATE ' || newtab || ' SET stddev_angle_after = ' || uhol || ' WHERE iternum = ' || iternum;
	EXECUTE 'SELECT min(uhol) FROM docasna' INTO uhol;
	EXECUTE 'UPDATE ' || newtab || ' SET min_angle_after = ' || uhol || ' WHERE iternum = ' || iternum;
	EXECUTE 'SELECT max(uhol) FROM docasna' INTO uhol;
	EXECUTE 'UPDATE ' || newtab || ' SET max_angle_after = ' || uhol || ' WHERE iternum = ' || iternum;
	EXECUTE 'SELECT count(*) FROM ' || tintable || '_optimizing_info_' || iternum || ' WHERE status = 1' INTO pom;
	EXECUTE 'UPDATE ' || newtab || ' SET flip_count = ' || pom || ' WHERE iternum = ' || iternum;
	DROP TABLE docasna;
END
$$
LANGUAGE plpgsql;
  
CREATE OR REPLACE FUNCTION tin_area3d(triangle geometry)
  RETURNS double precision AS
'$libdir/pg3angles-1.0', 'TIN_Area3D'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_centroid(triangle geometry)
  RETURNS geometry AS
'$libdir/pg3angles-1.0', 'TIN_Centroid'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_partialderivativesoftriangle(triangle geometry)
  RETURNS double precision[] AS
'$libdir/pg3angles-1.0', 'TIN_PartialDerivativesOfTriangle'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_slopeoftriangle(triangle geometry)
  RETURNS double precision AS
'$libdir/pg3angles-1.0', 'TIN_SlopeOfTriangle'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_aspectoftriangle(triangle geometry)
  RETURNS double precision AS
'$libdir/pg3angles-1.0', 'TIN_AspectOfTriangle'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_linearz(triangle geometry, point geometry)
  RETURNS double precision AS
'$libdir/pg3angles-1.0', 'TIN_LinearZ'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_extendm(triangle geometry, tidentifier double precision)
  RETURNS geometry AS
'$libdir/pg3angles-1.0', 'TIN_ExtendM'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_gettid(triangle geometry)
  RETURNS integer AS
'$libdir/pg3angles-1.0', 'TIN_GetTid'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_editneighboursinfo(trianglesarray geometry[], triangle geometry)
  RETURNS geometry AS
'$libdir/pg3angles-1.0', 'TIN_EditNeighboursInfo'
  LANGUAGE c STRICT; 
  
CREATE OR REPLACE FUNCTION tin_getneighbourstids(triangle geometry)
  RETURNS integer[] AS
'$libdir/pg3angles-1.0', 'TIN_GetNeighboursTids'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_flip(triangles geometry[])
  RETURNS geometry[] AS
'$libdir/pg3angles-1.0', 'TIN_Flip'
  LANGUAGE c STRICT; 
  
CREATE OR REPLACE FUNCTION tin_partialderivativesonvertex(point geometry, triangles geometry[])
  RETURNS double precision[] AS
'$libdir/pg3angles-1.0', 'TIN_PartialDerivativesOnVertex'
  LANGUAGE c STRICT;  

CREATE OR REPLACE FUNCTION tin_slopeonvertex(point geometry, triangles geometry[])
  RETURNS double precision AS
'$libdir/pg3angles-1.0', 'TIN_SlopeOnVertex'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_aspectonvertex(point geometry, triangles geometry[])
  RETURNS double precision AS
'$libdir/pg3angles-1.0', 'TIN_AspectOnVertex'
  LANGUAGE c STRICT;

CREATE OR REPLACE FUNCTION tin_quasiquadraticz(triangle geometry, triangles1 geometry[], triangles2 geometry[], triangles3 geometry[], point geometry)
  RETURNS double precision AS
'$libdir/pg3angles-1.0', 'TIN_QuasiQuadraticZ'
  LANGUAGE c STRICT;

CREATE OR REPLACE FUNCTION tin_quasiquadraticderivatives(triangle geometry, triangles1 geometry[], triangles2 geometry[], triangles3 geometry[], point geometry)
  RETURNS double precision[] AS
'$libdir/pg3angles-1.0', 'TIN_QuasiQuadraticDerivatives'
  LANGUAGE c STRICT;

CREATE OR REPLACE FUNCTION tin_normalsangle(normal1 double precision[], normal2 double precision[])
  RETURNS double precision AS
'$libdir/pg3angles-1.0', 'TIN_NormalsAngle'
  LANGUAGE c  STRICT;
  
CREATE OR REPLACE FUNCTION tin_isconvexhull(triangle1 geometry, triangle2 geometry)
  RETURNS boolean AS
'$libdir/pg3angles-1.0', 'TIN_IsConvexHull'
  LANGUAGE c STRICT;
  
CREATE OR REPLACE FUNCTION tin_contours(triangle geometry, deltaz double precision)
  RETURNS geometry[] AS
'$libdir/pg3angles-1.0', 'TIN_Contours'
  LANGUAGE c STRICT;