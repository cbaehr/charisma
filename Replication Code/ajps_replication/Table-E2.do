
/*##################################################################
# Partial replication files for "Parties as Disciplinarians: Charisma and Commitment Problems in Programmatic Campaigning" 
#by James R. Hollyer, Marko Klasnja and Rocio Titiunik, forthcoming at AJPS in 2021.
##################################################################*/


/* This do-file replicates Table E2: Total Campaign Receipts Ratio, Mayoral vs. 
	Local Council Candidates. 
	
	Data are from: https://www.tse.jus.br/hotsites/pesquisas-eleitorais/prestacao_contas.html 
	
	For the file to run, the following user-written programs are required: 
	
	-frmttable- (can be found by typing 'search sg97_5', then clicking on the package link 
		and following the instructions to install). */
	
use "brazil-receipts-candidates-data", clear

collapse (mean) mean=ratio (median) median=ratio (sd) sd=ratio, by(year)
mkmat mean-sd, mat(ratio)

frmttable, hlines(11{0}1) statfont(fs11) ///
	statmat(ratio) substat(0) sdec(2) ///
	rtitles("2004" \ "2008" \ "2012") ///
	ctitles("Year","Mean","Median","S.D.") ///
	title("\caption{Total Campaign Receipts Ratio, Mayoral vs. Local Council Candidates\label{tab:ratio}}\leavevmode") ///
	titlfont(normalsize)

