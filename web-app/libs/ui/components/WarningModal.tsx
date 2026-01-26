import { useTranslation } from '../provider';
import { Button } from './Button';
import { Icon } from './Icon';
import { Label } from './Label';
import { Modal } from './Modal';
import { Widget } from './Widget';

export interface WarningModalProps {
  className?: string;
  onResolve?: (value: boolean) => void;
  Open: boolean;
}

export const WarningModal: React.FC<WarningModalProps> = ({
  onResolve,
  Open,
}) => {
  const t = useTranslation('ui.warningModal');

  return (
    <Modal layout="vertical" open={Open} onClose={() => onResolve?.(false)} size="xs">
      <Modal.Content size="sm" className="flex flex-col justify-between items-center gap-2">
        <div className="flex justify-end w-full">
          <Icon name="xmark" className="cursor-pointer" onClick={() => onResolve?.(false)} />
        </div>
        <div className="p-6 rounded-full bg-error-bg">
          <Icon name="triangle-exclamation" size="2xl" className="text-error-text" />
        </div>

        <Label size="lg" className="font-medium">
          {t('areYouSure')}
        </Label>
       
          <Label className="text-center text-text-grey" size="xs">
            {t('thisActionCannotBeUndone')}
          </Label>
        

        <Widget
          corner="none"
          variant="error"
          circle={false}
          contentAlignment="center"
          title={
            <>
              <Icon name="circle-info" /> {t('warning')}
            </>
          }
          titleClassName="text-center font-medium"
          subtitle={t('warningText')}
          subtitleClassName="text-xs text-center"
          className="hover:shadow-none"
        />

        <div className="!mt-[20px] flex gap-3">
          <Button size="sm" className="w-[150px]" onClick={() => onResolve?.(false)}>
            {t('no')}
          </Button>
          <Button
            outline="primary"
            size="sm"
            className="w-[150px]"
            onClick={() => onResolve?.(true)}
          >
            {t('yes')}
          </Button>
        </div>
      </Modal.Content>
    </Modal>
  );
};
