 // also works for gzip compressed files
 //  String filename =  "path/to/pdbfile.ent" ;
import org.biojava.bio.structure.io.PDBFileReader;
import org.biojava.bio.structure.Structure;

public class PDBTest {
  public static void main(String[] args) {
    for (String filename: args) {
      PDBFileReader pdbreader = new PDBFileReader();
           
      try{
        Structure struc = pdbreader.getStructure(filename);
        Runtime runtime = Runtime.getRuntime();
            // Run the garbage collector
            //     runtime.gc();
        // Calculate the used memory
        // long memory = runtime.totalMemory() - runtime.freeMemory();
        long memory = runtime.totalMemory() / 1024 / 1024;
        System.out.println("Used memory is megabytes: " + memory);
      } catch (Exception e){
        e.printStackTrace();
      }
    }
  }
}
