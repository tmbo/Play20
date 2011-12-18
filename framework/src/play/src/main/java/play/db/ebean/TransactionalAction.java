package play.db.ebean;

import play.mvc.*;
import play.mvc.Http.*;

import com.avaje.ebean.*;

/**
 * Wraps an action in am Ebean transaction.
 */
public class TransactionalAction extends Action<Transactional> {
    
    public Result call(final Context ctx) throws Throwable {
        return Ebean.execute(new TxCallable<Result>() {  
            public Result call() {
                try {
                    return deleguate.call(ctx);
                } catch(RuntimeException e) {
                    throw e;
                } catch(Throwable t) {
                    throw new RuntimeException(t);
                }
            }
        });
    }
    
}