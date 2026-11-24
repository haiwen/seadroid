/*
 * Copyright 2016. SHENQINCI(沈钦赐)<946736079@qq.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.seafile.seadroid2.view;

import android.text.Editable;
import android.text.TextWatcher;
import android.widget.EditText;

import androidx.annotation.NonNull;

import java.util.Stack;

/**
 * Undo and redo.
 * Created by Shen Qinci on 16/6/23.
 */
public class PerformEdit {
    // Operation sequence number (a single edit may map to several operations, e.g. replacing
    // text is a delete plus an insert).
    int index;
    // Maximum number of undo records kept, to prevent memory growth while editing large files.
    private static final int MAX_HISTORY_SIZE = 50;
    // Undo stack.
    Stack<Action> history = new Stack<>();
    // Redo stack.
    Stack<Action> historyBack = new Stack<>();

    private Editable editable;
    private EditText editText;
    // Automatic-operation flag, used to avoid re-entrant callbacks that would cause endless undo.
    private boolean flag = false;
    // Whether undo history is recorded. Set to false while filling text in bulk so the loading
    // process is not mistaken for user edits.
    private boolean enabled = true;

    public PerformEdit(@NonNull EditText editText) {
        CheckNull(editText, "EditText must not be null");
        this.editable = editText.getText();
        this.editText = editText;
        editText.addTextChangedListener(new Watcher());
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    protected void onEditableChanged(Editable s) {

    }

    protected void onTextChanged(Editable s) {

    }


    /**
     * Clear records.
     * Clear history.
     */
    public final void clearHistory() {
        history.clear();
        historyBack.clear();
    }


    /**
     * Undo.
     */
    public final void undo() {
        if (history.empty()) return;
        // Lock operations.
        flag = true;
        Action action = history.pop();
        historyBack.push(action);
        if (action.isAdd) {
            // Undo an insertion.
            editable.delete(action.startCursor, action.startCursor + action.actionTarget.length());
            editText.setSelection(action.startCursor, action.startCursor);
        } else {
            // Undo a deletion.
            editable.insert(action.startCursor, action.actionTarget);
            if (action.endCursor == action.startCursor) {
                editText.setSelection(action.startCursor + action.actionTarget.length());
            } else {
                editText.setSelection(action.startCursor, action.endCursor);
            }
        }
        // Release the lock.
        flag = false;
        // Keep undoing while the next action belongs to the same operation.
        if (!history.empty() && history.peek().index == action.index) {
            undo();
        }
    }

    /**
     * Redo.
     */
    public final void redo() {
        if (historyBack.empty()) return;
        flag = true;
        Action action = historyBack.pop();
        history.push(action);
        if (action.isAdd) {
            // Redo an insertion.
            editable.insert(action.startCursor, action.actionTarget);
            if (action.endCursor == action.startCursor) {
                editText.setSelection(action.startCursor + action.actionTarget.length());
            } else {
                editText.setSelection(action.startCursor, action.endCursor);
            }
        } else {
            // Redo a deletion.
            editable.delete(action.startCursor, action.startCursor + action.actionTarget.length());
            editText.setSelection(action.startCursor, action.startCursor);
        }
        flag = false;
        // Check whether the next action belongs to the same operation.
        if (!historyBack.empty() && historyBack.peek().index == action.index)
            redo();
    }

    /**
     * Set the text for the first time.
     * Set default text.
     */
    public final void setDefaultText(CharSequence text) {
        clearHistory();
        flag = true;
        editable.replace(0, editable.length(), text);
        flag = false;
    }

    private class Watcher implements TextWatcher {

        /**
         * Before text changed.
         *
         * @param s     the s
         * @param start the start, i.e. the initial cursor position
         * @param count the endCursor, i.e. the number of selected characters
         * @param after the after, i.e. the number of characters added by the replacement
         */
        @Override
        public final void beforeTextChanged(CharSequence s, int start, int count, int after) {
            if (flag || !enabled) return;
            int end = start + count;
            if (end > start && end <= s.length()) {
                CharSequence charSequence = s.subSequence(start, end);
                // Text was deleted.
                if (charSequence.length() > 0) {
                    Action action = new Action(charSequence, start, false);
                    if (count > 1) {
                        // More than one character at once means the user selected text and then
                        // replaced or deleted it.
                        action.setSelectCount(count);
                    } else if (count == 1 && count == after) {
                        // A single character was replaced.
                        action.setSelectCount(count);
                    }
                    // Another case: select one character and then delete it (not handled yet).

                    history.push(action);
                    historyBack.clear();
                    action.setIndex(++index);
                    trimHistory();
                }
            }
        }

        /**
         * On text changed.
         *
         * @param s      the s
         * @param start  the start, i.e. the initial cursor position
         * @param before the before, i.e. the number of selected characters
         * @param count  the endCursor, i.e. the number of added characters
         */
        @Override
        public final void onTextChanged(CharSequence s, int start, int before, int count) {
            if (flag || !enabled) return;
            int end = start + count;
            if (end > start) {
                CharSequence charSequence = s.subSequence(start, end);
                // Text was added.
                if (charSequence.length() > 0) {
                    Action action = new Action(charSequence, start, true);

                    history.push(action);
                    historyBack.clear();
                    if (before > 0) {
                        // Text replacement (delete first, then insert). The delete and the insert
                        // belong to the same operation, so the sequence number is not incremented.
                        action.setIndex(index);
                    } else {
                        action.setIndex(++index);
                    }
                    trimHistory();
                }
            }
        }

        @Override
        public final void afterTextChanged(Editable s) {
            if (flag) return;
            if (s != editable) {
                editable = s;
                onEditableChanged(s);
            }
            PerformEdit.this.onTextChanged(s);
        }

    }

    private class Action {
        /**
         * The changed characters.
         */
        CharSequence actionTarget;
        /**
         * Cursor position.
         */
        int startCursor;
        int endCursor;
        /**
         * Whether this is an insert operation.
         */
        boolean isAdd;
        /**
         * Operation sequence number.
         */
        int index;


        public Action(CharSequence actionTag, int startCursor, boolean add) {
            this.actionTarget = actionTag;
            this.startCursor = startCursor;
            this.endCursor = startCursor;
            this.isAdd = add;
        }

        public void setSelectCount(int count) {
            this.endCursor = endCursor + count;
        }

        public void setIndex(int index) {
            this.index = index;
        }
    }


    private void trimHistory() {
        while (history.size() > MAX_HISTORY_SIZE) {
            history.remove(0);
        }
        while (historyBack.size() > MAX_HISTORY_SIZE) {
            historyBack.remove(0);
        }
    }

    private static void CheckNull(Object o, String message) {
        if (o == null) throw new IllegalStateException(message);
    }
}
