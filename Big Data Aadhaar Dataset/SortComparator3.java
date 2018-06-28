import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.WritableComparable;
import org.apache.hadoop.io.WritableComparator;

public class SortComparator3 extends WritableComparator {
	
	@Override
	public int compare(WritableComparable key1, WritableComparable key2) {
		IntWritable value1 = (IntWritable) key1;
		IntWritable value2 = (IntWritable) key2;
		
		return value1.get() < value2.get() ? -1 : value1.get() == value2.get() ? 0 : 1; 
		
	}

	protected SortComparator3() {
        super(IntWritable.class, true);
    }
}