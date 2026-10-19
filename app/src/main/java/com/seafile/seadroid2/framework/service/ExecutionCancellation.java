package com.seafile.seadroid2.framework.service;

import java.io.IOException;
import java.util.concurrent.CancellationException;

import retrofit2.Call;
import retrofit2.Response;

/**
 * Execution-scoped metadata cancellation; no shared-client cancelAll and no global stop flag.
 */
public final class ExecutionCancellation {
    private static final ThreadLocal<ExecutionCancellation> CURRENT = new ThreadLocal<>();
    private boolean cancelled;
    private Call<?> call;

    public static void enter(ExecutionCancellation token) {
        CURRENT.set(token);
    }

    public static void leave() {
        CURRENT.remove();
    }

    public synchronized void cancel() {
        cancelled = true;
        notifyAll();
        if (call != null) call.cancel();
    }

    public static void check() {
        ExecutionCancellation token = CURRENT.get();
        if (token != null) synchronized (token) {
            if (token.cancelled) throw new CancellationException("Execution stopped");
        }
    }

    /**
     * Wait on the background execution, waking immediately when its owner is cancelled.
     */
    public static void await(long millis) {
        ExecutionCancellation token = CURRENT.get();
        if (token == null) token = new ExecutionCancellation();
        token.awaitDelay(millis);
    }

    private synchronized void awaitDelay(long millis) {
        long remaining = java.util.concurrent.TimeUnit.MILLISECONDS.toNanos(millis);
        long deadline = System.nanoTime() + remaining;
        while (!cancelled && remaining > 0) {
            try {
                java.util.concurrent.TimeUnit.NANOSECONDS.timedWait(this, remaining);
            } catch (InterruptedException error) {
                Thread.currentThread().interrupt();
                throw new CancellationException("Execution interrupted");
            }
            remaining = deadline - System.nanoTime();
        }
        if (cancelled) throw new CancellationException("Execution stopped");
    }

    public static <T> Response<T> execute(Call<T> call) throws IOException {
        ExecutionCancellation token = CURRENT.get();
        if (token == null) {
            return call.execute();
        }

        synchronized (token) {
            if (token.cancelled) {
                call.cancel();
                throw new CancellationException("Execution stopped");
            }
            token.call = call;
        }
        try {
            return call.execute();
        } finally {
            synchronized (token) {
                if (token.call == call) {
                    token.call = null;
                }
            }
        }
    }
}
