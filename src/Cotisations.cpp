#include "Simulation.h"
#include "OutilsBase.h"
#include "DroitsRetr.h"
#include "Retraite.h"

const vector<int> NC_NonTit = {S_NC, S_NONTIT}; //// nombre de cotisants non titulaires ?

double CotMalRetrComp(Indiv& X, int age) { //// Cotisation Maladie payée sur la Retraite Complémentaire
    int t = X.anaiss%1900 + age;  //// t = année courante
    if(X.retr->pension_ar>0 || X.retr->pension_ag>0||X.retr->pension_ag_ar>0) { //// Si la pension AGIRC ou la pension ARRCO ou la pension AGIRC-ARRCO est non nulle
        return M->TauxMalComp[t] * (X.retr->pension_ag + X.retr->pension_ar+X.retr->pension_ag_ar);       //// renvoie le montant de la cotisation maladie payée sur les pensions AGIRC, ARRCO et AGIRC-ARRCO
    }
    return 0.0;
}

double CSGRet(Indiv& X, int age)  //// CSG payée sur la pension de retraite
{
    int t = X.anaiss%1900 + age; //// t = année courante
    if (X.retr->pension_tot > M->SeuilExoCSG[t]) //// Si la pension de retraite totale est supérieure au seuil d'exonération de CSG
        return M->TauxCSGRetFort[t]*X.retr->pension_tot; //// renvoie le montant de CSG payée sur la pension.
    else
        return 0;
}

double CSGSal(Indiv& X, int age)  //// CSG payée sur le salaire
{
    int t = X.anaiss%1900 + age;
    return M->TauxCSGSal[t]*X.salaires[age];
}

double CotAut(Indiv & X, int age) //// Cotisations Autres que la retraite lorsque X a age ans.
{
    int t = X.anaiss%1900 + age;
	
	
    if(in(X.statuts[age], Statuts_FP)) //// Si X est fonctionnaire
	//// renvoie le montant de cotisation, qui se décompose en 
        return  M->TauxMalPubTot[t] * X.salaires[age] //// cotisation maladie payée sur l'ensemble du salaire (traitement + primes)
              + M->TauxMalPubTr[t]  * X.salaires[age] / (1+(X.taux_prim)) //// + cotisation maladie payée sur le traitement sans les primes
			        + M->Taux_FDS[t]* part(X.salaires[age]-M->TauxFP[t]*X.salaires[age]/(1+X.taux_prim), 0 , 4*M->PlafondSS[t]) //// + cotisation au fonds de solidarité payée sur la partie du salaire diminuée des cotisations (totales ?) sur le traitement hors primes qui dépasse 4 plafonds de sécurité sociale...
			        *(X.salaires[age]-M->TauxFP[t]*X.salaires[age]/(1+X.taux_prim)>309*M->PointFP[t])?1:0; ////... uniquement si cette partie dépasse 309 fois la valeur du point d'indice.
    else if(X.statuts[age] == S_CAD) { //// Si X est un cadre du secteur privé
        return 	//// renvoie le montant de cotisation, qui se décompose en 
             M->TauxMalTot[t] * X.salaires[age] //// cotisation maladie sur l'ensemble du salaire
            +M->TauxMalSP[t]  * part(X.salaires[age], 0 ,   M->PlafondSS[t]) //// + cotisation maladie sur la part de salaire inférieure au plafond de la sécurité sociale
            +M->TauxAssedic[t]* part(X.salaires[age], 0 , 4*M->PlafondSS[t]) //// + cotisation Assedic (chômage) sur la partie de salaire inférieure à 4 plafonds de la sécurité sociale
			      +M->Taux_APEC[t]*part(X.salaires[age], 0 , 4*M->PlafondSS[t]); //approximation avant 2011 en dessous de 1 PSS part forfaitaire //// + cotisation APEC (assoc pour l'emploi des cadres) sur la part de salaire supérieure à 4 plafonds
			}
			else if (X.statuts[age] == S_NC){ //// Si X est salarié non cadre du privé
			return //// renvoie le montant de cotisation, qui esg identique à celui d'un cadre de même salaire sans la cotisation APEC
			      M->TauxMalTot[t] * X.salaires[age] //// 
            +M->TauxMalSP[t]  * part(X.salaires[age], 0 ,   M->PlafondSS[t])
            +M->TauxAssedic[t]* part(X.salaires[age],  0 , 4*M->PlafondSS[t]);
			}
			else return //// Si X est dans une autre situation (independant), renvoie le montant des cotisations
			(M->TauxMalRSI[t]-0.035*(1-part(X.salaires[age], 0, 0.7*M->PlafondSS[t])/(0.7*M->PlafondSS[t])))*part(X.salaires[age], 0, 0.7*M->PlafondSS[t]) //// [Taux cotisation maladie RSI - 3,5% du rapport entre la part de revenu d'activité inférieure à 70% du plafond de la sécurité sociale] * part du revenu d'activité inférieure à 70% du PSS  
            +M->TauxMalRSI[t]*(X.salaires[age]-part(X.salaires[age], 0, 0.7*M->PlafondSS[t])); //formule vraie à partir de 2017 (avant donne une approximation) //// + Taux cotisation maladie RSI appliqué au reste du revenu d'activité
}


double CotRet(Indiv & X, int age) //// Cotisations retraite (part salariale) lorsque X a age ans.
{
   double cot=0;
   int a = age;
   int t = X.anaiss%1900 + age;
   
   double effetLegislationARRCO_bis = (t < 119 || options->anLeg <2015 || options->NoAccordAgircArrco || options->NoRegUniqAgircArrco) ? M->TauxARRCO_S1[t]*M->TauxAppARRCO[t]*part(X.salaires[a],               0,  M->PlafondSS[t]) : 0; //// SI année < 2019 OU législation antérieure à 2015 OU accord AGIRC-ARRCO de 2015 non appliqué OU fusion AGIRC-ARRCO non effective en 2019, ALORS taux ARRCO sur tranche inférieure au plafond de la sécurité sociale multiplié par le taux d'appel ARRCO appliqué à la tranche de revenu inférieure au PSS, SINON 0.
   double effetLegislationARRCO_ter = (t >= 119 && options->anLeg >=2015 && !options->NoAccordAgircArrco && !options->NoRegUniqAgircArrco) ? M->TauxAGIRC_ARRCO_S1[t]*M->TauxAppAGIRC_ARRCO[t]*part(X.salaires[a],               0,  M->PlafondSS[t]) : 0; //// SI cas contraire, ALORS taux AGIRC-ARRCO sur la 1e tranche multiplié par taux d'appel AGIRC-ARRCO appliqué ) la tranche de revenu inférieure au plafond de la sécurité sociale, SINON 0.
   double effetLegislationARRCO = ((t < 119 || options->anLeg <2015 || options->NoAccordAgircArrco || options->NoRegUniqAgircArrco) && in(X.statuts[age], NC_NonTit)) ? M->TauxARRCO_S2[t]*M->TauxAppARRCO[t]*part(X.salaires[a],  M->PlafondSS[t],3*M->PlafondSS[t]) : 0; //// Si premier cas ET contractuel (? : normalement, S_NONTIT signifie contractuel), ALORS taux ARRCO sur la 2e tranche multiplié par le taux d'appel ARRCO appliqué à la tranche comprise entre 1 et 3 plafonds de la sécurité sociale, SINON 0.
   double effetLegislationAGIRC = ((t < 119 || options->anLeg <2015 || options->NoAccordAgircArrco || options->NoRegUniqAgircArrco) && X.statuts[age]==S_CAD) ? M->TauxAGIRC_SB[t]*M->TauxAppAGIRC[t]*part(X.salaires[a],  M->PlafondSS[t],4*M->PlafondSS[t]) + M->TauxAGIRC_SC[t]*M->TauxAppAGIRC[t]*part(X.salaires[a],4*M->PlafondSS[t],8*M->PlafondSS[t]) : 0; //// SI premier cas ET cadre du privé, alors taux AGIRC multiplié par taux d'appel AGIRC appliqué à la part du salaire comprise entre 1 et 4 plafonds de la sécurité sociale.
   double effetLegislationAGIRC_ARRCO = (t >= 119 && options->anLeg >=2015 && !options->NoAccordAgircArrco && !options->NoRegUniqAgircArrco) ? M->TauxAGIRC_ARRCO_S2[t] * M->TauxAppAGIRC_ARRCO[t] * part(X.salaires[a], M->PlafondSS[t],8*M->PlafondSS[t]) : 0; //// Si cas contraire (2e cas), ALORS taux AGIRC-ARRCO sur la 2e tranche multiplié par taux d'appel AGIRC-ARRCO appliqué à la tranche de revenu comprise entre 1 et 8 plafonds de la sécurité sociale.
   double effetLegislation_CAD = (t >= 116 && options->anLeg >=2015 && !options->NoAccordAgircArrco) ? M->TauxAGFF_S2[t] * part(X.salaires[age],4*M->PlafondSS[t] , 8*M->PlafondSS[t]) : 0; //// SI année >=2016 ET législation >=2015 ET accord AGIRC-ARRCO de 2015 appliqué ET si fusion AGIRC-ARRCO effective en 2019, ALORS taux AGFF sur la 2e tranche appliqué à la part de salaire comprise entre 4 et 8 plafonds de la sécurité sociale.
   double effetLegislation_NON_CAD = (t >= 119 && options->anLeg >=2015 && !options->NoAccordAgircArrco) ? M->TauxAGFF_S2[t] * part(X.salaires[age],3*M->PlafondSS[t] , 4*M->PlafondSS[t]) : 0; //// SI année >=2019 ET législation >=2015 ET si fusion AGIRC-ARRCO effective en 2019, ALORS taux AGFF sur la 2e tranche appliqué à la part de salaire comprise entre 3 et 4 plafonds de la sécurité sociale.
   //double effetLegislationCET = (t >= 119 && options->anLeg >=2015 && !options->NoAccordAgircArrco) ? M->TauxCET_S[t] * part(X.salaires[age],0 , 8*M->PlafondSS[t]) : 0;
   
   
   
   if (X.statuts[a]==S_NC || X.statuts[a]==S_NONTIT || X.statuts[a]==S_CAD){ //// Si X est salarié (cadre ou non-cadre) ou contractuel,
       cot= M->TauxSalRGSalTot[t]*X.salaires[a] //// cotisation = Taux de la cotisation salariale déplafonnée du régime général appliqué à l'ensemble du salaire 
            + M->TauxSalRGSP[t] *part(X.salaires[a], 0,  M->PlafondSS[t]) //// + taux de cotisation salariale plafonnée appliqué à la part du salaire inférieure au plafond de la sécurité sociale
			      + effetLegislationARRCO //// + diverses cotisation AGIRC-ARRCO détaillées ci-dessus
			      + effetLegislationARRCO_bis
			      + effetLegislationARRCO_ter
			      + effetLegislationAGIRC
			      + effetLegislationAGIRC_ARRCO;
		if(X.statuts[age] == S_CAD) {	cot+=            +M->TauxAGFF_S1[t] * part(X.salaires[age],  0 ,   M->PlafondSS[t]) // Si X est cadre, on applique en plus le taux AGFF de 1e tranche à la part du salaire inférieure au plafond de la sécurité sociale
            + M->TauxAGFF_S2[t] * part(X.salaires[age],M->PlafondSS[t] , 4*M->PlafondSS[t]) // et on applique en outre le taux AGFF de 2e tranche à la part de salaire comprise entre 1 et 4 plafonds.
			      + effetLegislation_CAD // extension du taux AGFF de la tranche B à C à compter du 01/01/16
		        + M->TauxCET_S[t] * part(X.salaires[age],0 , 8*M->PlafondSS[t]);} // et on applique enfin le taux de Cotisation Exceptionnelle Temporaire à la part de salaire inférieure à 8 plafonds.
		else {cot+= (in(X.statuts[age], NC_NonTit)) * (M->TauxAGFF_S1[t] * part(X.salaires[age],  0 ,   M->PlafondSS[t]) //// Si X est contractuel, on applique le taux AGFF 1e tranche à la part de salaire inférieure au plafond de la sécurité sociale.
              + M->TauxAGFF_S2[t] * part(X.salaires[age],M->PlafondSS[t] , 3*M->PlafondSS[t]) //// Si X est salarié non-cadre ou contractuel, on applique le taux AGFF 2e tranche à la part de salaire comprise entre 1 et 3 plafonds.
			        + effetLegislation_NON_CAD);}
      
	}
	
   if (in(X.statuts[a],Statuts_FP)) //// Si X est fonctionnaire,
      cot=M->TauxFP[t]*X.salaires[a]/(1+X.taux_prim); //// cotisation = Taux de cotisation de fonctionnaire appliqué au traitement hors primes
   if (X.statuts[a] == S_IND) //// Si X est indépendant,
      cot= M->TauxRSIsurP[t]*(X.salaires[a]-part(X.salaires[a],0,M->PlafondSS[t])) //// cotisation = Taux de la cotisation applicable aux revenus d'activité dépassant le plafond appliqué à ladite part de revenu
            +M->TauxRSIssP[t]*part(X.salaires[a],0,M->PlafondSS[t]); //// et taux de cotisation applicable aux revenus d'activité sous le plafond appliqué à ladite part de revenu.
   
   return cot;  
}



   
double CotRetrPatr(Indiv & X, int age) { //// Cotisation retraite patronale lorsque X a age ans
    int t = X.anaiss%1900 + age;
	int a =age;
    double cot=0;
	//// idem que pour la part salariale, mais avec la part patronale. Il y a quelques différences.
   double effetLegislationARRCO_bis = (t < 119 || options->anLeg <2015 || options->NoAccordAgircArrco || options->NoRegUniqAgircArrco) ? (M->TauxARRCO_1[t] - M->TauxARRCO_S1[t])*M->TauxAppARRCO[t]*part(X.salaires[a],               0,  M->PlafondSS[t]) : 0;
   double effetLegislationARRCO_ter = (t >= 119 && options->anLeg >=2015 && !options->NoAccordAgircArrco && !options->NoRegUniqAgircArrco) ? (M->TauxAGIRC_ARRCO_1[t] -M->TauxAGIRC_ARRCO_S1[t])*M->TauxAppAGIRC_ARRCO[t]*part(X.salaires[a],               0,  M->PlafondSS[t]) : 0;
   double effetLegislationARRCO = ((t < 119 || options->anLeg <2015 || options->NoAccordAgircArrco || options->NoRegUniqAgircArrco) && in(X.statuts[age], NC_NonTit)) ? (M->TauxARRCO_2[t] -M->TauxARRCO_S2[t])*M->TauxAppARRCO[t]*part(X.salaires[a],  M->PlafondSS[t],3*M->PlafondSS[t]) : 0;
   double effetLegislationAGIRC = ((t < 119 || options->anLeg <2015 || options->NoAccordAgircArrco || options->NoRegUniqAgircArrco) && X.statuts[age]==S_CAD) ? (M->TauxAGIRC_B[t] -M->TauxAGIRC_SB[t])*M->TauxAppAGIRC[t]*part(X.salaires[a],  M->PlafondSS[t],4*M->PlafondSS[t]) + (M->TauxAGIRC_C[t] -M->TauxAGIRC_SC[t])*M->TauxAppAGIRC[t]*part(X.salaires[a],4*M->PlafondSS[t],8*M->PlafondSS[t]) : 0;
   double effetLegislationAGIRC_ARRCO = (t >= 119 && options->anLeg >=2015 && !options->NoAccordAgircArrco && !options->NoRegUniqAgircArrco) ? (M->TauxAGIRC_ARRCO_2[t] -M->TauxAGIRC_ARRCO_S2[t]) * M->TauxAppAGIRC_ARRCO[t] * part(X.salaires[a], M->PlafondSS[t],8*M->PlafondSS[t]) : 0;
   //double effetLegislationCET = ((t < 119 || options->anLeg <2015 || options->NoAccordAgircArrco) && X.statuts[age]==S_CAD) ? (M->TauxCET[t] - M->TauxCET_S[t]) * part(X.salaires[age],0 , 8*M->PlafondSS[t]) : 0;
   //double effetLegislationCET_bis = (t >= 119 && options->anLeg >=2015 && !options->NoAccordAgircArrco) ? (M->TauxCET[t] - M->TauxCET_S[t]) * part(X.salaires[age],0 , 8*M->PlafondSS[t]) : 0;
   double cotisCET= X.statuts[age]==S_CAD ? (M->TauxCET[t] - M->TauxCET_S[t]) * part(X.salaires[age],0 , 8*M->PlafondSS[t]) : 0;
   double effetLegislationAGFF_2 = ((t < 119 || options->anLeg <2015 || options->NoAccordAgircArrco) && X.statuts[age]==S_CAD) ? (M->TauxAGFF_2[t]-M->TauxAGFF_S2[t]) * part(X.salaires[age],M->PlafondSS[t] , 4*M->PlafondSS[t]) : 0;
   double effetLegislationAGFF_2_bis = ((t < 119 || options->anLeg <2015 || options->NoAccordAgircArrco) && in(X.statuts[age], NC_NonTit)) ? (M->TauxAGFF_2[t]-M->TauxAGFF_S2[t]) * part(X.salaires[age],M->PlafondSS[t] , 3*M->PlafondSS[t]) : 0;
   double effetLegislationAGFF_2_ter = (t >= 119 && options->anLeg >=2015 && !options->NoAccordAgircArrco && X.statuts[age]==S_CAD) ? (M->TauxAGFF_2[t]-M->TauxAGFF_S2[t]) * part(X.salaires[age],M->PlafondSS[t] , 8*M->PlafondSS[t]) : 0;
   double effetLegislationAGFF_2_quater = (t >= 119 && options->anLeg >=2015 && !options->NoAccordAgircArrco && in(X.statuts[age], NC_NonTit)) ? (M->TauxAGFF_2[t]-M->TauxAGFF_S2[t]) * part(X.salaires[age],M->PlafondSS[t] , 4*M->PlafondSS[t]) : 0;
   
    if (in(X.statuts[age], NC_NonTit) || X.statuts[age] == S_CAD)  // A corriger pour prendre en compte taux de cotisation Ircantec différent de celui de l'ARRCO
    {
       cot= M->TauxEmpRGSalTot[t] * X.salaires[age]
            +M->TauxEmpRGSP[t] *                                           part(X.salaires[age],               0,    M->PlafondSS[t])
            + effetLegislationARRCO_bis
			+ effetLegislationARRCO_ter
			+ effetLegislationARRCO
			+ effetLegislationAGIRC
			+ effetLegislationAGIRC_ARRCO
			+(M->TauxAGFF_1[t] -M->TauxAGFF_S1[t]) * part(X.salaires[age],             0 ,   M->PlafondSS[t])
            + effetLegislationAGFF_2
			+ effetLegislationAGFF_2_bis
			+ effetLegislationAGFF_2_ter
			+ effetLegislationAGFF_2_quater
			+ cotisCET;
    }
	
	//// La différence principale vient de ce que la part "patronale" varie selon que l'on appartient à la fonction publique d'Etat, ou territoriale et hospitalière.		
    if (in(X.statuts[age],Statuts_FPE))
    {
       cot =M->TauxEmplFPE[t] * X.salaires[age] / (1 + X.taux_prim);
    }
    if (in(X.statuts[age],Statuts_FPT))
    {
       cot =M->TauxEmplFPTH[t] * X.salaires[age] / (1 + X.taux_prim);     // FPT ou FPH -> on applique le taux moyen CNRACL
    }
    if (X.statuts[age] == S_IND)
    {
       cot= M->TauxEmpRGSalTot[t] * X.salaires[age]
            +M->TauxEmpRGSP[t] * part(X.salaires[age], 0, M->PlafondSS[t]);
    }
   
    return cot;
}

double SalNet(Indiv& X, int age) { //// Salaire net de l'individu X, obtenu en deduisant les cotisations salariales retraite, autres, et la CSG.
    return X.salaires[age] - CotRet(X, age) - CotAut(X, age) - CSGSal(X, age);
}

double SalMoy(Indiv& X, int age, int nb_an) { //// Moyenne des salaires nets d'un individu X d'age ans sur les nb_an dernières années, si cette période commence après le 14e anniversaire de X ; sinon 0.
    double denom(0), num(0);
    int t = X.date(age);
    
    for(int a = age; a >= 14; a--) {
        if(in(X.statuts[a], Statuts_occ)) {
            
            denom++;
            num += SalNet(X,a) * M->SMPT[t] / M->SMPT[X.date(a)]; //// Attention, le salaire net de chaque année est ramené à ce qu'aurait été sa valeur l'année où X a age ans.
            if(--nb_an == 0)
                return num/denom;
            
        }
    }
    
    return 0;
}

double PNet(Indiv& X, int age) { //// Pension nette de l'individu X d'age ans, obtenue en déduisant la CSG et la cotisation maladie payée sur la retraite complémentaire.
  if(X.retr->pension_tot)
    return X.retr->pension_tot - CSGRet(X,age) - CotMalRetrComp(X,age);
  else
    return 0;
}

double TauxRemp(Indiv& X, int age) { //// Taux de remplacement de l'individu X d'age ans = rapport entre pension nette et salaire net SEULEMENT SI X prend sa retraite cette année (X ne peut pas être retraité depuis plus d'un an). Sinon 0.
    double denom = SalMoy(X,age,1); //// Manière détournée d'obtenir le salaire net courant SI X est bien en activité (et non déjà à la retraite). Sinon 0.
    double num = PNet(X,age);
    if(denom > 0)
        return num / denom;
    return 0;
}

double cotAGFFTot(Indiv& X, int age) { //// Cotisation AGFF totale (part salariale et patronale)
int t = X.anaiss%1900 + age;
	double effetLegislation_CAD = (t >= 116 && options->anLeg >=2015 && !options->NoAccordAgircArrco) ? M->TauxAGFF_2[t] * part(X.salaires[age],4*M->PlafondSS[t] , 8*M->PlafondSS[t]) : 0;
	double effetLegislation_NON_CAD = (t >= 119 && options->anLeg >=2015 && !options->NoAccordAgircArrco) ? M->TauxAGFF_2[t] * part(X.salaires[age],3*M->PlafondSS[t] , 8*M->PlafondSS[t]) : 0;
	
   if(X.statuts[age] == S_CAD) {
        return 
             M->TauxAGFF_1[t] * part(X.salaires[age],             0 ,   M->PlafondSS[t])
            +M->TauxAGFF_2[t] * part(X.salaires[age],M->PlafondSS[t] , 4*M->PlafondSS[t])
			+ effetLegislation_CAD; // extension du taux AGFF de la tranche B à C à compter du 01/01/16
			}
			else if(in(X.statuts[age], NC_NonTit)) {
			return 
             M->TauxAGFF_1[t] * part(X.salaires[age],             0 ,   M->PlafondSS[t])
            +M->TauxAGFF_2[t] * part(X.salaires[age],M->PlafondSS[t] , 3*M->PlafondSS[t])
			+ effetLegislation_NON_CAD;
			}
			else {return 0;}
}

// Destinie 2
// Copyright © 2005-2018, Institut national de la statistique et des études économiques
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
