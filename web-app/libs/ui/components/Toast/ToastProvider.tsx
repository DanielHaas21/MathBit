'use client';
import * as React from 'react';
import { Toast } from './Toast';
import { IconName } from '../../icons/names';
import { uid } from 'uid';

type ToastMessage = {
  id: string;
  title: React.ReactNode;
  description?: React.ReactNode;
  variant?: 'default' | 'success' | 'error' | 'warning';
  tint?: boolean;
  icon?: IconName;
  duration?: number;
  actions?: React.ReactNode;
  progressBar?: number;
};

type ToastContextType = {
  show: (toast: Omit<ToastMessage, 'id'>) => void;
};

const ToastContext = React.createContext<ToastContextType | null>(null);

export function ToastProvider({ children }: { children: React.ReactNode }) {
  const [toasts, setToasts] = React.useState<ToastMessage[]>([]);

  const show = React.useCallback((toast: Omit<ToastMessage, 'id'>) => {
    const id = uid();

    const newToast: ToastMessage = { id, ...toast };
    setToasts((prev) => [...prev, newToast]);

    if (toast.duration !== 0) {
      setTimeout(() => {
        setToasts((prev) => prev.filter((t) => t.id !== id));
      }, toast.duration ?? 5000);
    }
  }, []);

  const remove = React.useCallback((id: string) => {
    setToasts((prev) => prev.filter((t) => t.id !== id));
  }, []);

  return (
    <ToastContext.Provider value={{ show }}>
      {children}
      <div className="fixed bottom-8 right-4 z-50">
        <Toast>
          {toasts.map((t) => (
            <Toast.Item
              key={t.id}
              variant={t.variant ?? 'success'}
              icon={t.icon as any}
              description={t.description}
              tint={t.tint ?? true}
              progressBar={t.progressBar}
              actions={t.actions}
              onRemove={() => remove(t.id)}
            >
              {t.title}
            </Toast.Item>
          ))}
        </Toast>
      </div>
    </ToastContext.Provider>
  );
}

export function useToast() {
  const ctx = React.useContext(ToastContext);
  if (!ctx) throw new Error('useToast must be used inside <ToastProvider>');
  return ctx;
}
