




global pulso_social "/fs0/home/tc57/Pulso social/" 
global geih "${pulso_social}GEIH/"
global input "${geih}Input/"
global output "${geih}Output/"


global variables "ocupados desocupados formales informales inglab_p25 inglab_p50 inglab_p75 inac inac_estudia inac_hogar inac_incap inac_otro"
global indicadores "tasa_desempleo tasa_formal tasa_informal ninis tasa_part inglab_p25  inglab_p50  inglab_p75"


local total=0
local minoria=0
local indic_export=1


/********************************* 
*** PROCESAR BASES CON TOTALES ***
**********************************/ 


if `total'==1	{

	* Alistar base ocupados formales-informales

	use "${input}append_POSICION_OCUPACIONAL_TOTAL.dta", clear

	reshape wide ocupados, i(year month area edad sexo) j(posicion)

		rename (ocupados1 ocupados4) (formales informales)
		label variable formales "Obreros o empleados de empresas particulares"	
		label variable informales "Cuentas propias"

	tempfile formalidad
	save `formalidad'


	* Alistar base ingresos laborales

	use "${input}append_INGLABO_2010_2019.dta", clear

	reshape wide inglabo, i(year month area edad sexo) j(percentil) string
		
		rename inglabo* inglab_*

		label variable inglab_p25 "Ingresos laborales percentil 25"	
		label variable inglab_p50 "Ingresos laborales percentil 50"	
		label variable inglab_p75 "Ingresos laborales percentil 75"	

	tempfile inglabo
	save `inglabo'


	* Alistar base inactivos 

	use "${input}append_INACTIVOS_2010_2020.dta", clear

	reshape wide inactivos, i(year month area edad sexo) j(tipo_inactivos)

		rename (inactivos3 inactivos4 inactivos5 inactivos6) (inac_estudia inac_hogar inac_incap inac_otro)
		egen inac=rowtotal(inac_*)

		label variable inac "inactivos"
		label variable inac_estudia "Inactivos que estudian"	
		label variable inac_hogar "Inactivos que se dedican a oficios del hogar"	
		label variable inac_incap "Inactivos incapacitados permanentes para trabajar"	
		label variable inac_otro "Inactivos que no estudian, no se dedican al hogar, ni están incapacitados"	

	tempfile inactivos
	save `inactivos'

	
	* Juntar todas las bases de datos

	use "${input}append_DESOCUPADOS_TOTAL.dta", clear

	merge 1:1 year month area edad sexo using "${input}append_OCUPADOS_TOTAL.dta
		drop _merge
	
	merge 1:1 year month area edad sexo using `formalidad'
		drop _merge

	merge 1:1 year month area edad sexo using `inactivos'
		drop _merge

	merge 1:1 year month area edad sexo using `inglabo'
		drop _merge
		
		// Base a nivel de año, mes, area metropolitana y sexo
	reshape wide $variables, i(year month area edad) j(sexo)
		rename *1 *_hombr
		rename *2 *_muj
	
		// Base a nivel de año, mes y area metropolitana	
	reshape wide *_hombr *_muj, i(year month area) j(edad)
		rename *1 *_jov
		rename *2 *_adult	

	// Crear variables agregadas (total, mujeres, hombres, jovenes) por area metropolitana (sin desgregar por genero y edad)
	foreach var of global variables	 {
		egen `var'_tot=rowtotal(`var'*)
		gen `var'_muj=`var'_muj_jov+`var'_muj_adult
		gen `var'_hombr=`var'_hombr_jov+`var'_hombr_adult
		gen `var'_jov=`var'_muj_jov+`var'_hombr_jov
		gen `var'_adult=`var'_muj_adult+`var'_hombr_adult	
	}		

	save "${input}temp_total.dta", replace

}

/***************************** 
*** PROCESAR BASE MINORIAS ***
*****************************/ 

if `minoria'==1	{


	* Alistar base ocupados formales-informales

	use "${input}append_POSICION_MINORIAS_oct_dic_2020.dta", clear

	reshape wide ocupados, i(year month area edad sexo minoria) j(posicion)

		rename (ocupados1 ocupados4) (formales informales)
		label variable formales "Obreros o empleados de empresas particulares"	
		label variable informales "Cuentas propias"

	tempfile formalidad_minorias
	save `formalidad_minorias'


	* Alistar base ingresos laborales

	use "${input}append_INGLABO_MINORIAS_OCT_DIC_2020.dta", clear

	reshape wide inglabo, i(year month area edad sexo minoria) j(percentil) string

		rename inglabo* inglab_*

		label variable inglab_p25 "Ingresos laborales percentil 25"	
		label variable inglab_p50 "Ingresos laborales percentil 50"	
		label variable inglab_p75 "Ingresos laborales percentil 75"	

	tempfile inglabo_minorias
	save `inglabo_minorias'


	* Alistar base inactivos 

	use "${input}append_INACTIVOS_MINORIAS_OCT_DIC_2020.dta", clear

	reshape wide inactivos, i(year month area edad sexo minoria) j(tipo_inactivos)

		rename (inactivos3 inactivos4 inactivos5 inactivos6) (inac_estudia inac_hogar inac_incap inac_otro)
		egen inac=rowtotal(inac_*)

		label variable inac "Inactivos"
		label variable inac_estudia "Inactivos que estudian"	
		label variable inac_hogar "Inactivos que se dedican a oficios del hogar"	
		label variable inac_incap "Inactivos incapacitados permanentes para trabajar"	
		label variable inac_otro "Inactivos que no estudian, no se dedican al hogar, ni están incapacitados"	

	tempfile inactivos_minorias
	save `inactivos_minorias'

	
	* Juntar todas las bases de datos

	/* Nota: Es normal que hallan varios datos missing para minorías, en unos meses específicos, para grupos específicos.
	Esto es porque la GEIH pierde representatividad a nivel de area metropolitana cuando se definen subgrupos muy pequeños.
	En consecuencia, al hacer el merge puede que no peguen varias observaciones  */

	use "${input}append_DESOCUPADOS_MINORIAS_oct_dic_2020.dta", clear

	merge 1:1 year month area edad sexo minoria using "${input}append_OCUPADOS_MINORIAS_oct_dic_2020.dta
		drop _merge

	merge 1:1 year month area edad sexo minoria using `formalidad_minorias'
		drop _merge

	merge 1:1 year month area edad sexo minoria using `inactivos_minorias'
		drop _merge
	
	merge 1:1 year month area edad sexo minoria using `inglabo_minorias'
		drop _merge
		
		// Base a nivel de año, mes, area metropolitana, edad y minoria
	reshape wide $variables, i(year month area edad minoria) j(sexo)
		rename *1 *_hombr
		rename *2 *_muj

		// Base a nivel de año, mes, area metropolitana y minoria	
	reshape wide *_hombr *_muj, i(year month area minoria) j(edad)
		rename *1 *_jov
		rename *2 *_adult	

		// Base a nivel de año, mes y area metropolitana
	reshape wide *_jov *_adult, i(year month area) j(minoria)
		rename *0 *_min
		rename *1 *_nomin

	* Crear indicadores para minorias
	
	// Crear variables para minorias, por area metropolitana (total y por edad)
	foreach var of global variables	 {
		egen `var'_jov_min=rowtotal(`var'*jov_min)
		egen `var'_jov_nomin=rowtotal(`var'*jov_nomin)
		egen `var'_adult_min=rowtotal(`var'*adult_min)
		egen `var'_adult_nomin=rowtotal(`var'*adult_nomin)
		gen `var'_min=`var'_jov_min+`var'_adult_min
		gen `var'_nomin=`var'_jov_nomin+`var'_adult_nomin

	}

	*drop *_muj* *_hombr*

	save "${input}temp_minorias.dta", replace

}

/****************************************************
**** CREAR INDICADORES Y EXPORTAR BASES DE DATOS ****
****************************************************/

if `indic_export'==1		{

	* Unir bases de datos con totales y minorias

	use "${input}temp_total.dta", clear

	merge 1:1 year month area using "${input}temp_minorias.dta"


	* Crear indicadores
		
	// Crear pea, desempleo, tasa de formalidad e informalidad, pct ninis, participacion laboral	
	foreach cat in tot muj hombr jov adult muj_jov muj_adult hombr_jov hombr_adult min nomin jov_min jov_nomin adult_min adult_nomin  {	
		gen pob_eco_activa_`cat'=desocupados_`cat'+ocupados_`cat'
		gen tasa_desempleo_`cat'=desocupados_`cat'/pob_eco_activa_`cat'*100
		gen tasa_formal_`cat'=formales_`cat'/ocupados_`cat'*100
		gen tasa_informal_`cat'=informales_`cat'/ocupados_`cat'*100
		gen pob_edad_trabajar_`cat'=inac_`cat'+pob_eco_activa_`cat'
		gen ninis_`cat'=(desocupados_`cat'+inac_`cat'-inac_estudia_`cat')/pob_edad_trabajar_`cat'*100
		gen tasa_part_`cat'=pob_eco_activa_`cat'/pob_edad_trabajar_`cat'*100
	}	

	// Hallar promedio mensual
	collapse (mean) tasa_desempleo* tasa_formal* tasa_informal* ninis* tasa_part* inglab*, by(year area) 

	// Crear base de datos a nivel de año, ciudad y categoria
	reshape long $indicadores, i(year area) j(cat) string
	
	// Crear variable para nombre de area metropolitana
	rename area cod_area
	gen area="Total Nacional" if cod_area=="00"
	replace area="Medellín" if cod_area=="05"
	replace area="Barranquilla" if cod_area=="08"
	replace area="Bogotá, D.C" if cod_area=="11"
	replace area="Cartagena" if cod_area=="13"
	replace area="Manizales" if cod_area=="17"
	replace area="Montería" if cod_area=="23"
	replace area="Villavicencio" if cod_area=="50"
	replace area="Pasto" if cod_area=="52"
	replace area="Cúcuta" if cod_area=="54"
	replace area="Pereira" if cod_area=="66"
	replace area="Bucaramanga" if cod_area=="68"
	replace area="Ibagué" if cod_area=="73"
	replace area="Cali" if cod_area=="76"

	// Crear label para categorias
	gen cat_label=" (Mujeres)" if cat=="_muj"
	replace cat_label=" (Mujeres Jóvenes, 14-28 años)" if cat=="_muj_jov"
	replace cat_label=" (Mujeres Adultas, 29-64 años)" if cat=="_muj_adult"
	replace cat_label=" (Hombres)" if cat=="_hombr"
	replace cat_label=" (Hombres Jóvenes, 14-28 años)" if cat=="_hombr_jov"
	replace cat_label=" (Hombres Adultos, 29-64 años)" if cat=="_hombr_adult"
	replace cat_label=" (Jóvenes, 14-28 años)" if cat=="_jov"
	replace cat_label=" (Adultos, 29-64 años)" if cat=="_adult"
	replace cat_label=" (Minorias étnicas)" if cat=="_min"
	replace cat_label=" (No minorias)" if cat=="_nomin"
	replace cat_label=" (Jóvenes Minorias étnicas, 14-28 años)" if cat=="jov_min"
	replace cat_label=" (Jóvenes No minorias, 14-28 años)" if cat=="jov_nomin"
	replace cat_label=" (Adultos Minorias étnicas, 29-64 años)" if cat=="adult_min"
	replace cat_label=" (Adultos No minorias, 29-64 años)" if cat=="adult_nomin"
	replace cat_label="" if cat=="_tot"

	// Crear algunas variables identificadoras
	gen id_data=1
	gen id_time=1
	gen time=year


	*foreach var in tasa_part		{
	foreach var of global indicadores	{

		gen value=`var'
		if "`var'"=="tasa_desempleo" gen value_label="Tasa de desempleo (%)"+cat_label
		else if "`var'"=="tasa_formal" gen value_label="Tasa de formalidad (%)"+cat_label
		else if "`var'"=="tasa_informal" gen value_label="Tasa de informalidad (%)"+cat_label
		else if "`var'"=="ninis" gen value_label="Porcentaje de la población en edad de trabajar que no trabaja, ni estudia (%)"+cat_label
		else if "`var'"=="tasa_part" gen value_label="Participacion laboral (%)"+cat_label
		else if "`var'"=="inglab_p25" gen value_label="Ingreso laboral percentil 25 (pesos)"+cat_label
		else if "`var'"=="inglab_p50" gen value_label="Ingreso laboral percentil 50 (pesos)"+cat_label
		else if "`var'"=="inglab_p75" gen value_label="Ingreso laboral percentil 75 (pesos)"+cat_label


	*** EXPORTAR BASES A NIVEL NACIONAL ***


	* guardar bases de datos con total nacional

		di "***`var', nacional"
		preserve
		keep if cod_area=="00"
		keep if cat=="_tot"
		gen variable="`var'"
		gen id_nivel="nacional"
		gen nivel_value=1
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_col_2010-2020.csv", comma replace
		restore

	* guardar bases de datos con variables para adultos, total nacional

		di "***`var', nacional, adultos"
		preserve
		keep if cod_area=="00"
		keep if inlist(cat,"_adult")
		gen variable="`var'_adulto"
		gen id_nivel="nacional"
		gen nivel_value=1
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_adulto_col_2010-2020.csv", comma replace
		restore

	* guardar bases de datos con variables para jóvenes, total nacional

		di "***`var', nacional, jóvenes"
		preserve
		keep if cod_area=="00"
		keep if inlist(cat,"_jov")
		gen variable="`var'_joven"
		gen id_nivel="nacional"
		gen nivel_value=1
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_joven_col_2010-2020.csv", comma replace
		restore


	* guardar bases de datos con variables por genero, total nacional

		di "***`var', nacional, por genero"
		preserve
		keep if cod_area=="00"
		keep if inlist(cat,"_muj","_hombr")
		gen variable="`var'"
		gen id_nivel="nacional_gen"
		gen nivel_value="1_1" if cat=="_hombr"
		replace nivel_value="1_2" if cat=="_muj"
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_col_gen_2010-2020.csv", comma replace
		restore

* guardar bases de datos con variables para adultos por genero, total nacional

		di "***`var', nacional, adultos, por genero"
		preserve
		keep if cod_area=="00"
		keep if inlist(cat,"_muj_adult","_hombr_adult")
		gen variable="`var'_adulto"
		gen id_nivel="nacional_gen"
		gen nivel_value="1_1" if cat=="_hombr_adult"
		replace nivel_value="1_2" if cat=="_muj_adult"
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_adulto_col_gen_2010-2020.csv", comma replace
		restore

* guardar bases de datos con variables para jóvenes por genero, total nacional

		di "***`var', nacional, jóvenes, por género"
		preserve
		keep if cod_area=="00"
		keep if inlist(cat,"_muj_jov","_hombr_jov")
		gen variable="`var'_joven"
		gen id_nivel="nacional_gen"
		gen nivel_value="1_1" if cat=="_hombr_jov"
		replace nivel_value="1_2" if cat=="_muj_jov"
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_joven_col_gen_2010-2020.csv", comma replace
		restore

	* guardar bases de datos con variables por minoria, total nacion

		di "***`var', nacional, por minoria"
		preserve
		keep if cod_area=="00"
		keep if inlist(cat,"_min","_nomin")
		keep if year=="2020"
		gen variable="`var'"
		gen id_nivel="nacional_etnia"
		gen nivel_value="1_1" if cat=="_min"
		replace nivel_value="1_2" if cat=="_nomin"
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_col_etnia_2020.csv", comma replace
		restore

	* guardar bases de datos con variables para adultos por minoria, total nacional

		di "***`var', nacional, adultos, por minoria"
		preserve
		keep if cod_area=="00"
		keep if inlist(cat,"_adult_min","_adult_nomin")
		keep if year=="2020"
		gen variable="`var'_adulto"
		gen id_nivel="nacional_etnia"
		gen nivel_value="1_1" if cat=="adult_min"
		replace nivel_value="1_2" if cat=="adult_nomin"
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_adulto_col_etnia_2020.csv", comma replace
		restore

	* guardar bases de datos con variables para jóvenes por minoria, total nacional

		di "***`var', nacional, jóvenes, por minoria"
		preserve
		keep if cod_area=="00"
		keep if inlist(cat,"_jov_min","_jov_nomin")
		keep if year=="2020"
		gen variable="`var'_joven"
		gen id_nivel="nacional_etnia"
		gen nivel_value="1_1" if cat=="_jov_min"
		replace nivel_value="1_2" if cat=="_jov_nomin"
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_joven_col_etnia_2020.csv", comma replace
		restore


	*** EXPORTAR BASES A NIVEL DE AREA METROPOLITANA ***

	* guardar bases de datos con totales, a nivel de area metropolitana
	
		di "***`var', por area"
		preserve
		keep if cod_area!="00"
		keep if cat=="_tot"
		gen variable="`var'"
		gen id_nivel="ciudad"
		gen nivel_value=area
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_ciudad_2010-2020.csv", comma replace
		restore

	* guardar bases de datos con variables para adultos, a nivel de area metropolitana

		di "***`var', por area, adultos"
		preserve
		keep if cod_area!="00"
		keep if inlist(cat,"_adult")
		gen variable="`var'_adulto"
		gen id_nivel="ciudad"
		gen nivel_value=area
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_adulto_ciudad_2010-2020.csv", comma replace
		restore

	* guardar bases de datos con variables para jovenes, a nivel de area metropolitana

		di "***`var', por area, jóvenes"
		preserve
		keep if cod_area!="00"
		keep if inlist(cat,"_jov")
		gen variable="`var'_joven"
		gen id_nivel="ciudad"
		gen nivel_value=area
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_joven_ciudad_2010-2020.csv", comma replace
		restore

	* guardar bases de datos con variables por genero, a nivel de area metropolitana

		di "***`var', por area, por genero"
		preserve
		keep if inlist(cat,"_muj","_hombr")
		gen variable="`var'"
		gen id_nivel="ciudad_gen"
		gen nivel_value=area+"_1" if cat=="_hombr"
		replace nivel_value=area+"_2" if cat=="_muj"
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_ciudad_gen_2010-2020.csv", comma replace
		restore

	* guardar bases de datos con variables para adultos por genero, a nivel de area metropolitana

		di "***`var', por area, adultos, por genero"
		preserve
		keep if cod_area!="00"
		keep if inlist(cat,"_muj_adult","_hombr_adult")
		gen variable="`var'_adulto"
		gen id_nivel="ciudad_gen"
		gen nivel_value=area+"_1" if cat=="_hombr_adult"
		replace nivel_value=area+"_2" if cat=="_muj_adult"
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_adulto_ciudad_gen_2010-2020.csv", comma replace
		restore

	* guardar bases de datos con variables para jóvenes por genero, a nivel de area metropolitana

		di "***`var', por area, jóvenes, por genero"
		preserve
		keep if cod_area!="00"
		keep if inlist(cat,"_muj_jov","_hombr_jov")
		gen variable="`var'_joven"
		gen id_nivel="ciudad_gen"
		gen nivel_value=area+"_1" if cat=="_hombr_jov"
		replace nivel_value=area+"_2" if cat=="_muj_jov"
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_joven_ciudad_gen_2010-2020.csv", comma replace
		restore

	* guardar bases de datos con variables por minoria etnica, a nivel de area metropolitana

		di "***`var', por area, por minoría"
		preserve
		keep if inlist(cat,"_min","_nomin")
		keep if year=="2020"
		gen variable="`var'"
		gen id_nivel="ciudad_etnia"
		gen nivel_value=area+"_1" if cat=="_min"
		replace nivel_value=area+"_2" if cat=="_nomin"
		outsheet id_data id_nivel nivel_value id_time time variable value value_label using "${output}base_`var'_ciudad_etnia_2020.csv", comma replace
		restore

	drop value value_label

	}
		
}