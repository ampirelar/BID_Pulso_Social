

local import=0
local clean=1


cd "/fs0/home/tc57/"



if `import'==1	{
	import delimited using "Casos_positivos_de_COVID-19_en_Colombia.csv", varnames(1)	
		rename (cãdigodivipoladepartamento cãdigodivipolamunicipio nombredelgrupo fechadediagnãsstico) (cod_dpto cod_ciudad etnia fechadiagnostico)
	save "casoscovid", replace
}

if `clean'==1	{

	use "casoscovid", clear

		gen minoria=missing(etnia)==0
		gen fallecido=regexm(lower(recuperado),"fallecido")

		* Fecha de diagnostico
		gen temp=subinstr(fechadiagnostico,"0:00:00","",.)
		gen fechadiag=date(temp,"DMY")
		format fechadiag %td 
		gen mes=month(fechadiag)
		gen ano=year(fechadiag)
		drop temp

		* Casos por mes/municipio
		sort cod_ciudad mes ano
		by cod_ciudad mes ano: egen casos_mpio=count(iddecaso)
		by cod_ciudad mes ano: egen muertes_mpio=total(fallecido)

		* Base de datos por municipio/mes
		egen tag=tag(cod_ciudad mes ano)
		keep if tag==1
		keep cod_ciudad cod_dpto mes ano casos_mpio muertes_mpio

	save "casoscovid_mpio", replace

	export delimited using "casoscovid_mpio", replace


}		