package com.seafile.seadroid2.framework.service;

import org.junit.Test;
import java.util.concurrent.CancellationException;
import static org.junit.Assert.*;

public class ExecutionCancellationTest {
    @Test public void cancellationIsOwnerScopedAndClearedBetweenExecutions() {
        ExecutionCancellation worker = new ExecutionCancellation();
        ExecutionCancellation ui = new ExecutionCancellation();
        worker.cancel();
        ExecutionCancellation.enter(ui);
        try { ExecutionCancellation.check(); } finally { ExecutionCancellation.leave(); }
        ExecutionCancellation.enter(worker);
        try { ExecutionCancellation.check(); fail(); }
        catch (CancellationException expected) { }
        finally { ExecutionCancellation.leave(); }
        ExecutionCancellation.check();
    }
    @Test public void repeatedCancellationIsIdempotent() {
        ExecutionCancellation token = new ExecutionCancellation();
        ExecutionCancellation.enter(token);
        try {
            token.cancel(); token.cancel();
            try { ExecutionCancellation.check(); fail(); }
            catch (CancellationException expected) { }
        } finally { ExecutionCancellation.leave(); }
    }
}
