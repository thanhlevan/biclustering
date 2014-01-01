package cs.kuleuven.java.mdl;

public class TernaryBiclusterMDL extends BiclusterMDLScore{
	
	private	int nSymbolSize;
	private String[] symbols;
	private int base;
	
	public TernaryBiclusterMDL() {
		super();
		nSymbolSize = 3;
		base = 2;
		symbols = new String[3];
		symbols[0] = "-1"; symbols[1] = "0"; symbols[2] = "1";
		
	}
	
	
	
	
	
	

}
